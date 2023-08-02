defmodule Livebook.Runtime.Evaluator do
  @moduledoc false

  # A process responsible for evaluating notebook code.
  #
  # When evaluator receives an evaluation request, it synchronously
  # evaluates the given code within itself, rather than spawning a
  # separate process. It stores the resulting binding and env in its
  # state (under a specific reference).
  #
  # Storing the binding in the same process that evaluates the code is
  # essential, because otherwise we would have to send it to another
  # process, which means copying a potentially massive amounts of data.
  #
  # Also, note that this process intentionally is not a GenServer,
  # because during evaluation we it may receive arbitrary messages
  # and we want to keep them in the inbox, whereas a GenServer would
  # always consume them.

  require Logger

  alias Livebook.Runtime.Evaluator

  @type t :: %{pid: pid(), ref: reference()}

  @type state :: %{
          evaluator_ref: reference(),
          io_proxy: pid(),
          io_proxy_monitor: reference(),
          send_to: pid(),
          runtime_broadcast_to: pid(),
          object_tracker: pid(),
          contexts: %{ref() => context()},
          initial_context: context(),
          initial_context_version: nil | (md5 :: binary()),
          ignored_pdict_keys: list(term())
        }

  @typedoc """
  An evaluation context.

  Each evaluation produces a  new context, which may be optionally
  used by a later evaluation.
  """
  @type context :: %{binding: Code.binding(), env: Macro.Env.t(), id: binary()}

  @typedoc """
  A term used to identify evaluation.
  """
  @type ref :: term()

  @typedoc """
  An evaluation result, either the return value or an error if
  raised.
  """
  @type evaluation_result ::
          {:ok, result :: any()}
          | {:error, Exception.kind(), error :: any(), Exception.stacktrace()}

  # We store some information in the process dictionary for non-blocking
  # access from other evaluators. In particular we store context metadata,
  # such as envs, this way we can build intellisense context without
  # asking the evaluator. We don't store binding though, because that
  # would take too much memory
  @evaluator_info_key :evaluator_info

  # We stor the path in process dictionary, so that the tracer can access it
  @ebin_path_key :ebin_path

  @doc """
  Starts an evaluator.

  ## Options

    * `:send_to` - the process to send evaluation messages to. Required

    * `:object_tracker` - a pid of `Livebook.Runtime.Evaluator.ObjectTracker`.
      Required

    * `:runtime_broadcast_to` - the process to send runtime broadcast
      events to. Defaults to the value of `:send_to`

    * `:ebin_path` - a directory to write modules bytecode into. When
      not specified, modules are not written to disk

    * `:io_proxy_registry` - the registry to register IO proxy
      processes in

  """
  @spec start_link(keyword()) :: {:ok, pid(), t()} | {:error, term()}
  def start_link(opts \\ []) do
    case :proc_lib.start_link(__MODULE__, :init, [opts]) do
      {:error, error} -> {:error, error}
      evaluator -> {:ok, evaluator.pid, evaluator}
    end
  end

  @doc """
  Computes the memory usage for the current node.
  """
  @spec memory() :: Livebook.Runtime.runtime_memory()
  def memory() do
    %{
      total: total,
      processes: processes,
      atom: atom,
      binary: binary,
      code: code,
      ets: ets
    } = Map.new(:erlang.memory())

    %{
      total: total,
      processes: processes,
      atom: atom,
      binary: binary,
      code: code,
      ets: ets,
      other: total - processes - atom - binary - code - ets
    }
  end

  @doc """
  Asynchronously parses and evaluates the given code.

  The resulting context (binding and env) is stored under `ref`. Any
  subsequent calls may specify `parent_refs` pointing to a sequence
  of previous evaluations, in which case the accumulated context is
  used as the entry point for evaluation.

  Any exceptions are captured and transformed into an error result.

  The evaluation result is formatted into an output and sent to the
  configured client (see `start_link/1`) together with metadata.

  See `Livebook.Runtime.evaluate_code/5` for the messages format and
  the list of available options.

  ## Options

    * `:on_finish` - a function to run when the evaluation is
      finished. The function receives `t:evaluation_result/0`
      as an argument

  """
  @spec evaluate_code(t(), :elixir | :erlang, ref(), list(ref()), keyword()) :: :ok
  def evaluate_code(evaluator, language, code, ref, parent_refs, opts \\ []) do
    cast(evaluator, {:evaluate_code, language, code, ref, parent_refs, opts})
  end

  @doc """
  Fetches the evaluation context (binding and env) for the given
  evaluation sequence.
  """
  @spec get_evaluation_context(t(), list(ref())) :: context()
  def get_evaluation_context(evaluator, parent_refs) do
    call(evaluator, {:get_evaluation_context, parent_refs})
  end

  @doc """
  Fetches an aggregated evaluation context from `source_evaluator`
  and caches it as the initial context for `evaluator`.

  The process dictionary is also copied to match `source_evaluator`.
  """
  @spec initialize_from(t(), t(), ref()) :: :ok
  def initialize_from(evaluator, source_evaluator, source_parent_refs) do
    call(evaluator, {:initialize_from, source_evaluator, source_parent_refs})
  end

  @doc """
  Removes the evaluation identified by `ref` from history.

  The corresponding context is removed and garbage collected.
  """
  @spec forget_evaluation(t(), ref()) :: :ok
  def forget_evaluation(evaluator, ref) do
    cast(evaluator, {:forget_evaluation, ref})
  end

  @doc """
  Returns an empty intellisense context.
  """
  @spec intellisense_context() :: Livebook.Intellisense.intellisense_context()
  def intellisense_context() do
    env = Code.env_for_eval([])
    map_binding = fn fun -> fun.([]) end
    %{env: env, map_binding: map_binding}
  end

  @doc """
  Builds intellisense context from the given evaluation.
  """
  @spec intellisense_context(t(), list(ref())) :: Livebook.Intellisense.intellisense_context()
  def intellisense_context(evaluator, parent_refs) do
    {:dictionary, dictionary} = Process.info(evaluator.pid, :dictionary)

    evaluator_info = find_in_dictionary(dictionary, @evaluator_info_key)
    %{initial_context: {_id, initial_env}} = evaluator_info

    env =
      List.foldr(parent_refs, initial_env, fn ref, prev_env ->
        case evaluator_info.contexts do
          %{^ref => {_id, env}} -> merge_env(prev_env, env)
          _ -> prev_env
        end
      end)

    map_binding = fn fun -> map_binding(evaluator, parent_refs, fun) end

    %{env: env, map_binding: map_binding}
  end

  defp find_in_dictionary(dictionary, key) do
    Enum.find_value(dictionary, fn
      {^key, value} -> value
      _pair -> nil
    end)
  end

  # Applies the given function to evaluation binding
  defp map_binding(evaluator, parent_refs, fun) do
    call(evaluator, {:map_binding, parent_refs, fun})
  end

  @doc """
  Runs the given function with binding and env of the given evaluation.

  Ths function runs within the evaluator process, so that no data
  is copied between processes, unless explicitly sent.
  """
  @spec peek_context(t(), list(ref()), (context() -> any())) :: :ok
  def peek_context(evaluator, parent_refs, fun) do
    cast(evaluator, {:peek_context, parent_refs, fun})
  end

  defp cast(evaluator, message) do
    send(evaluator.pid, {:cast, evaluator.ref, message})
    :ok
  end

  defp call(evaluator, message) do
    call_ref = Process.monitor(evaluator.pid)
    send(evaluator.pid, {:call, evaluator.ref, self(), call_ref, message})

    receive do
      {^call_ref, reply} ->
        reply

      {:DOWN, ^call_ref, _, _, reason} ->
        exit({reason, {__MODULE__, :call, [evaluator, message]}})
    end
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :temporary
    }
  end

  def init(opts) do
    send_to = Keyword.fetch!(opts, :send_to)
    runtime_broadcast_to = Keyword.get(opts, :runtime_broadcast_to, send_to)
    object_tracker = Keyword.fetch!(opts, :object_tracker)
    ebin_path = Keyword.get(opts, :ebin_path)
    io_proxy_registry = Keyword.get(opts, :io_proxy_registry)

    {:ok, io_proxy} =
      Evaluator.IOProxy.start(
        self(),
        send_to,
        runtime_broadcast_to,
        object_tracker,
        ebin_path,
        io_proxy_registry
      )

    io_proxy_monitor = Process.monitor(io_proxy)

    # Use the dedicated IO device as the group leader, so that
    # intercepts all :stdio requests and also handles Livebook
    # specific ones
    Process.group_leader(self(), io_proxy)

    evaluator_ref = make_ref()
    evaluator = %{pid: self(), ref: evaluator_ref}

    context = initial_context()

    Process.put(@evaluator_info_key, %{
      initial_context: {context.id, context.env},
      contexts: %{}
    })

    Process.put(@ebin_path_key, ebin_path)

    ignored_pdict_keys = MapSet.new([:rand_seed, :random_seed] ++ Process.get_keys())

    state = %{
      evaluator_ref: evaluator_ref,
      io_proxy: io_proxy,
      io_proxy_monitor: io_proxy_monitor,
      send_to: send_to,
      runtime_broadcast_to: runtime_broadcast_to,
      object_tracker: object_tracker,
      contexts: %{},
      initial_context: context,
      initial_context_version: nil,
      ignored_pdict_keys: ignored_pdict_keys
    }

    :proc_lib.init_ack(evaluator)

    loop(state)
  end

  defp loop(%{evaluator_ref: evaluator_ref} = state) do
    receive do
      {:call, ^evaluator_ref, pid, ref, message} ->
        {:reply, reply, state} = handle_call(message, pid, state)
        send(pid, {ref, reply})
        loop(state)

      {:cast, ^evaluator_ref, message} ->
        {:noreply, state} = handle_cast(message, state)
        loop(state)

      {:DOWN, ref, :process, _pid, reason} when ref == state.io_proxy_monitor ->
        exit(reason)
    end
  end

  defp initial_context() do
    env = Code.env_for_eval([])
    env = Macro.Env.prepend_tracer(env, Evaluator.Tracer)
    %{id: random_id(), binding: [], env: env, pdict: %{}}
  end

  defp handle_cast({:evaluate_code, language, code, ref, parent_refs, opts}, state) do
    do_evaluate_code(language, code, ref, parent_refs, opts, state)
  end

  defp handle_cast({:forget_evaluation, ref}, state) do
    do_forget_evaluation(ref, state)
  end

  defp handle_cast({:peek_context, parent_refs, fun}, state) do
    context = get_context(state, parent_refs)
    fun.(context)
    {:noreply, state}
  end

  defp handle_call({:get_evaluation_context, parent_refs}, _from, state) do
    context = get_context(state, parent_refs)
    {:reply, context, state}
  end

  defp handle_call({:initialize_from, source_evaluator, source_parent_refs}, _from, state) do
    {:dictionary, dictionary} = Process.info(source_evaluator.pid, :dictionary)

    evaluator_info = find_in_dictionary(dictionary, @evaluator_info_key)

    version =
      source_parent_refs
      |> Enum.map(fn ref ->
        with {id, _env} <- evaluator_info.contexts[ref], do: id
      end)
      |> :erlang.md5()

    state =
      if version == state.initial_context_version do
        state
      else
        context = Evaluator.get_evaluation_context(source_evaluator, source_parent_refs)

        update_evaluator_info(fn info ->
          put_in(info.initial_context, {context.id, context.env})
        end)

        %{state | initial_context: context, initial_context_version: version}
      end

    {:reply, :ok, state}
  end

  defp handle_call({:map_binding, parent_refs, fun}, _from, state) do
    context = get_context(state, parent_refs)
    result = fun.(context.binding)
    {:reply, result, state}
  end

  defp do_evaluate_code(language, code, ref, parent_refs, opts, state) do
    {old_context, state} = pop_in(state.contexts[ref])

    if old_context do
      for module <- old_context.env.context_modules do
        delete_module(module)
      end
    end

    # We remove the old context from state and jump to a tail-recursive
    # function. This way we are sure there is no reference to the old
    # state and we can garbage collect the old context before the evaluation
    continue_do_evaluate_code(language, code, ref, parent_refs, opts, state)
  end

  defp continue_do_evaluate_code(language, code, ref, parent_refs, opts, state) do
    :erlang.garbage_collect(self())

    Evaluator.ObjectTracker.remove_reference_sync(state.object_tracker, {self(), ref})

    context = get_context(state, parent_refs)
    file = Keyword.get(opts, :file, "nofile")
    context = put_in(context.env.file, file)

    Evaluator.IOProxy.before_evaluation(state.io_proxy, ref, file)

    set_pdict(context, state.ignored_pdict_keys)

    start_time = System.monotonic_time()
    {eval_result, code_markers} = eval(language, code, context.binding, context.env)
    evaluation_time_ms = time_diff_ms(start_time)

    %{tracer_info: tracer_info} = Evaluator.IOProxy.after_evaluation(state.io_proxy)

    {new_context, result, identifiers_used, identifiers_defined} =
      case eval_result do
        {:ok, value, binding, env} ->
          context_id = random_id()

          new_context = %{
            id: context_id,
            binding: binding,
            env: prune_env(env, tracer_info),
            pdict: current_pdict(state)
          }

          {identifiers_used, identifiers_defined} =
            identifier_dependencies(new_context, tracer_info, context)

          result = {:ok, value}
          {new_context, result, identifiers_used, identifiers_defined}

        {:error, kind, error, stacktrace} ->
          for {module, _} <- tracer_info.modules_defined do
            delete_module(module)
          end

          result = {:error, kind, error, stacktrace}
          identifiers_used = :unknown
          identifiers_defined = %{}
          # Empty context
          new_context = initial_context()
          {new_context, result, identifiers_used, identifiers_defined}
      end

    if ebin_path() do
      Livebook.Runtime.Evaluator.Doctests.run(new_context.env.context_modules, code)
    end

    state = put_context(state, ref, new_context)

    output = Evaluator.Formatter.format_result(result, language)

    metadata = %{
      errored: error_result?(result),
      interrupted: interrupt_result?(result),
      evaluation_time_ms: evaluation_time_ms,
      memory_usage: memory(),
      code_markers: code_markers,
      identifiers_used: identifiers_used,
      identifiers_defined: identifiers_defined
    }

    send(state.send_to, {:runtime_evaluation_response, ref, output, metadata})

    if on_finish = opts[:on_finish] do
      on_finish.(result)
    end

    :erlang.garbage_collect(self())

    {:noreply, state}
  end

  defp error_result?(result) when elem(result, 0) == :error, do: true
  defp error_result?(_result), do: false

  defp interrupt_result?({:error, _kind, error, _stacktrace})
       when is_struct(error, Kino.InterruptError),
       do: true

  defp interrupt_result?(_result), do: false

  defp do_forget_evaluation(ref, state) do
    {context, state} = pop_context(state, ref)

    if context do
      for module <- context.env.context_modules do
        delete_module(module)

        # And we immediately purge the newly deleted code
        :code.purge(module)
      end

      Evaluator.ObjectTracker.remove_reference_sync(state.object_tracker, {self(), ref})
    end

    continue_do_forget_evaluation(context != nil, state)
  end

  defp continue_do_forget_evaluation(context?, state) do
    if context? do
      :erlang.garbage_collect(self())
    end

    {:noreply, state}
  end

  defp put_context(state, ref, context) do
    update_evaluator_info(fn info ->
      put_in(info.contexts[ref], {context.id, context.env})
    end)

    put_in(state.contexts[ref], context)
  end

  defp pop_context(state, ref) do
    update_evaluator_info(fn info ->
      {_, info} = pop_in(info.contexts[ref])
      info
    end)

    pop_in(state.contexts[ref])
  end

  defp update_evaluator_info(fun) do
    info = Process.get(@evaluator_info_key)
    Process.put(@evaluator_info_key, fun.(info))
  end

  defp get_context(state, parent_refs) do
    List.foldr(parent_refs, state.initial_context, fn ref, prev_context ->
      if context = state.contexts[ref] do
        merge_context(prev_context, context)
      else
        prev_context
      end
    end)
  end

  defp set_pdict(context, ignored_pdict_keys) do
    for key <- Process.get_keys(),
        key not in ignored_pdict_keys,
        not Map.has_key?(context.pdict, key) do
      Process.delete(key)
    end

    for {key, value} <- context.pdict do
      Process.put(key, value)
    end
  end

  defp current_pdict(state) do
    for {key, value} <- Process.get(),
        key not in state.ignored_pdict_keys,
        do: {key, value},
        into: %{}
  end

  defp prune_env(env, tracer_info) do
    env
    |> Map.replace!(:aliases, Map.to_list(tracer_info.aliases_defined))
    |> Map.replace!(:requires, Enum.sort(tracer_info.requires_defined))
    |> Map.replace!(:context_modules, Map.keys(tracer_info.modules_defined))
  end

  defp merge_context(prev_context, context) do
    binding = merge_binding(prev_context.binding, context.binding)
    env = merge_env(prev_context.env, context.env)
    pdict = context.pdict
    %{id: random_id(), binding: binding, env: env, pdict: pdict}
  end

  defp merge_binding(prev_binding, binding) do
    binding_map = Map.new(binding)

    kept_binding =
      Enum.reject(prev_binding, fn {var, _value} ->
        Map.has_key?(binding_map, var)
      end)

    binding ++ kept_binding
  end

  defp merge_env(prev_env, env) do
    env
    |> Map.update!(:versioned_vars, fn versioned_vars ->
      Enum.uniq(Map.keys(prev_env.versioned_vars) ++ Map.keys(versioned_vars))
      |> Enum.with_index()
      |> Map.new()
    end)
    |> Map.update!(:aliases, &Keyword.merge(prev_env.aliases, &1))
    |> Map.update!(:requires, &:ordsets.union(prev_env.requires, &1))
    |> Map.update!(:context_modules, &(&1 ++ prev_env.context_modules))
  end

  defp eval(:elixir, code, binding, env) do
    {{result, extra_diagnostics}, diagnostics} =
      with_diagnostics([log: true], fn ->
        try do
          quoted = Code.string_to_quoted!(code, file: env.file)

          try do
            {value, binding, env} =
              Code.eval_quoted_with_env(quoted, binding, env, prune_binding: true)

            {:ok, value, binding, env}
          catch
            kind, error ->
              stacktrace = prune_stacktrace(:elixir_eval, __STACKTRACE__)
              {:error, kind, error, stacktrace}
          end
        catch
          kind, error ->
            {:error, kind, error, []}
        end
        |> case do
          {:ok, value, binding, env} ->
            {{:ok, value, binding, env}, []}

          {:error, kind, error, stacktrace} ->
            # Mimic a diagnostic for relevant errors where it's not
            # the case by default
            extra_diagnostics =
              if extra_diagnostic?(error) do
                [
                  %{
                    file: error.file,
                    severity: :error,
                    message: error.description,
                    position: error.line,
                    stacktrace: stacktrace
                  }
                ]
              else
                []
              end

            {{:error, kind, error, stacktrace}, extra_diagnostics}
        end
      end)

    code_markers =
      for diagnostic <- diagnostics ++ extra_diagnostics,
          # Ignore diagnostics from other evaluations, such as inner Code.eval_string/3
          diagnostic.file == env.file and diagnostic.file != "nofile" do
        %{
          line:
            case diagnostic.position do
              {line, _column} -> line
              line -> line
            end,
          description: diagnostic.message,
          severity: diagnostic.severity
        }
      end

    {result, code_markers}
  end

  defp eval(:erlang, code, binding, env) do
    try do
      erl_binding =
        Enum.reduce(binding, %{}, fn {name, value}, erl_binding ->
          :erl_eval.add_binding(elixir_to_erlang_var(name), value, erl_binding)
        end)

      with {:ok, tokens, _} <- :erl_scan.string(String.to_charlist(code), {1, 1}, [:text]),
           {:ok, parsed} <- :erl_parse.parse_exprs(tokens),
           {:value, result, new_erl_binding} <- :erl_eval.exprs(parsed, erl_binding) do
        # Simple heuristic to detect the used variables. We look at
        # the tokens and assume all var tokens are used variables.
        # This will not handle shadowing of variables in fun definitions
        # and will only work well enough for expressions, not for modules.
        used_vars =
          for {:var, _anno, name} <- tokens,
              do: {erlang_to_elixir_var(name), nil},
              into: MapSet.new(),
              uniq: true

        # Note that for Elixir we evaluate with :prune_binding, here
        # replicate the same behaviour for binding and env

        binding =
          new_erl_binding
          |> Map.drop(Map.keys(erl_binding))
          |> Enum.map(fn {name, value} ->
            {erlang_to_elixir_var(name), value}
          end)

        env =
          update_in(env.versioned_vars, fn versioned_vars ->
            versioned_vars
            |> Map.filter(fn {var, _} -> MapSet.member?(used_vars, var) end)
            |> Map.merge(
              binding
              |> Enum.with_index(Kernel.map_size(versioned_vars) + 1)
              |> Map.new(fn {{name, _value}, version} -> {{name, nil}, version} end)
            )
          end)

        {{:ok, result, binding, env}, []}
      else
        # Tokenizer error
        {:error, {location, module, description}, _end_loc} ->
          process_erlang_error(env, code, location, module, description)

        # Parser error
        {:error, {location, module, description}} ->
          process_erlang_error(env, code, location, module, description)
      end
    catch
      kind, error ->
        stacktrace = prune_stacktrace(:erl_eval, __STACKTRACE__)
        {{:error, kind, error, stacktrace}, []}
    end
  end

  defp process_erlang_error(env, code, location, module, description) do
    line = :erl_anno.line(location)

    formatted =
      module.format_error(description)
      |> :erlang.list_to_binary()

    code_marker = %{
      line: line,
      severity: :error,
      description: "#{module}: #{formatted}"
    }

    error_cons =
      case {module, description} do
        {:erl_parse, [~c"syntax error before: ", []]} ->
          &TokenMissingError.exception/1

        _ ->
          &SyntaxError.exception/1
      end

    error =
      error_cons.(
        file: env.file,
        line: line,
        column:
          case :erl_anno.column(location) do
            :undefined -> 1
            val -> val
          end,
        description: formatted,
        snippet: make_snippet(code, location)
      )

    {{:error, :error, error, []}, filter_erlang_code_markers([code_marker])}
  end

  defp make_snippet(code, location) do
    start_line = 1
    start_column = 1
    line = :erl_anno.line(location)

    case :erl_anno.column(location) do
      :undefined ->
        nil

      column ->
        lines = :string.split(code, "\n", :all)
        snippet = :lists.nth(line - start_line + 1, lines)

        offset =
          if line == start_line do
            column - start_column
          else
            column - 1
          end

        case :string.trim(code, :leading) do
          [] -> nil
          _ -> %{content: snippet, offset: offset}
        end
    end
  end

  defp elixir_to_erlang_var(name) do
    name
    |> :erlang.atom_to_binary()
    |> Macro.camelize()
    |> :erlang.binary_to_atom()
  end

  defp erlang_to_elixir_var(name) do
    name
    |> :erlang.atom_to_binary()
    |> Macro.underscore()
    |> :erlang.binary_to_atom()
  end

  defp filter_erlang_code_markers(code_markers) do
    Enum.reject(code_markers, &(&1.line == 0))
  end

  # TODO: remove once we require Elixir v1.15
  if Code.ensure_loaded?(Code) and function_exported?(Code, :with_diagnostics, 2) do
    defp with_diagnostics(opts, fun) do
      Code.with_diagnostics(opts, fun)
    end
  else
    defp with_diagnostics(_opts, fun) do
      {fun.(), []}
    end
  end

  defp extra_diagnostic?(%SyntaxError{}), do: true
  defp extra_diagnostic?(%TokenMissingError{}), do: true

  defp extra_diagnostic?(%CompileError{description: description}) do
    not String.contains?(description, "(errors have been logged)")
  end

  defp extra_diagnostic?(_error), do: false

  defp identifier_dependencies(context, tracer_info, prev_context) do
    identifiers_used = MapSet.new()
    identifiers_defined = %{}

    # Variables

    identifiers_used =
      for var <- vars_used(context, tracer_info, prev_context),
          do: {:variable, var},
          into: identifiers_used

    identifiers_used =
      for var <- tracer_info.undefined_vars,
          do: {:variable, var},
          into: identifiers_used

    identifiers_defined =
      for var <- vars_defined(context, prev_context),
          do: {{:variable, var}, context.id},
          into: identifiers_defined

    # Modules

    identifiers_used =
      for module <- tracer_info.modules_used,
          do: {:module, module},
          into: identifiers_used

    identifiers_defined =
      for {module, _vars} <- tracer_info.modules_defined,
          version = module.__info__(:md5),
          do: {{:module, module}, version},
          into: identifiers_defined

    # Aliases

    identifiers_used =
      for alias <- tracer_info.aliases_used,
          do: {:alias, alias},
          into: identifiers_used

    identifiers_defined =
      for {as, alias} <- tracer_info.aliases_defined,
          do: {{:alias, as}, alias},
          into: identifiers_defined

    # Requires

    identifiers_used =
      for module <- tracer_info.requires_used,
          do: {:require, module},
          into: identifiers_used

    identifiers_defined =
      for module <- tracer_info.requires_defined,
          do: {{:require, module}, :ok},
          into: identifiers_defined

    # Imports

    identifiers_used =
      if tracer_info.imports_used? or tracer_info.imports_defined? do
        # Imports are not always incremental, due to :except, so if
        # we define imports, we also implicitly rely on prior imports
        MapSet.put(identifiers_used, :imports)
      else
        identifiers_used
      end

    identifiers_defined =
      if tracer_info.imports_defined? do
        version = {:erlang.phash2(context.env.functions), :erlang.phash2(context.env.macros)}
        put_in(identifiers_defined[:imports], version)
      else
        identifiers_defined
      end

    # Process dictionary

    # Every evaluation depends on the pdict
    identifiers_used = MapSet.put(identifiers_used, :pdict)

    identifiers_defined =
      if context.pdict == prev_context.pdict do
        identifiers_defined
      else
        version = :erlang.phash2(context.pdict)
        put_in(identifiers_defined[:pdict], version)
      end

    {MapSet.to_list(identifiers_used), identifiers_defined}
  end

  defp vars_used(context, tracer_info, prev_context) do
    prev_vars =
      for {var, _version} <- prev_context.env.versioned_vars,
          into: MapSet.new(),
          do: var

    outer_used_vars =
      for {var, _version} <- context.env.versioned_vars,
          into: MapSet.new(),
          do: var

    # Note that :prune_binding removes variables used by modules
    # (unless used outside), so we get those from the tracer
    module_used_vars =
      for {_module, vars} <- tracer_info.modules_defined,
          var <- vars,
          into: MapSet.new(),
          do: var

    # We take an intersection with previous vars, so we ignore variables
    # that we know are newly defined
    MapSet.intersection(prev_vars, MapSet.union(outer_used_vars, module_used_vars))
  end

  defp vars_defined(context, prev_context) do
    prev_num_vars = map_size(prev_context.env.versioned_vars)

    for {var, version} <- context.env.versioned_vars,
        version >= prev_num_vars,
        into: MapSet.new(),
        do: var
  end

  defp prune_stacktrace(_module, [{Livebook.Runtime.Evaluator.Tracer, _fun, _arity, _meta} | _]),
    do: []

  # Adapted from https://github.com/elixir-lang/elixir/blob/1c1654c88adfdbef38ff07fc30f6fbd34a542c07/lib/iex/lib/iex/evaluator.ex#L355-L372
  # TODO: Remove else branch once we depend on the versions below
  if System.otp_release() >= "25" do
    defp prune_stacktrace(module, stack) do
      stack
      |> Enum.reverse()
      |> Enum.drop_while(&(elem(&1, 0) != module))
      |> Enum.reverse()
      |> case do
        [] -> stack
        stack -> stack
      end
    end
  else
    @elixir_internals [:elixir, :elixir_expand, :elixir_compiler, :elixir_module] ++
                        [:elixir_clauses, :elixir_lexical, :elixir_def, :elixir_map] ++
                        [:elixir_erl, :elixir_erl_clauses, :elixir_erl_pass]

    defp prune_stacktrace(_, stacktrace) do
      # The order in which each drop_while is listed is important.
      # For example, the user may call Code.eval_string/2 in their
      # code and if there is an error we should not remove erl_eval
      # and eval_bits information from the user stacktrace.
      stacktrace
      |> Enum.reverse()
      |> Enum.drop_while(&(elem(&1, 0) == :proc_lib))
      |> Enum.drop_while(&(elem(&1, 0) == :gen_server))
      |> Enum.drop_while(&(elem(&1, 0) == __MODULE__))
      |> Enum.drop_while(&(elem(&1, 0) == :elixir))
      |> Enum.drop_while(&(elem(&1, 0) in [:erl_eval, :eval_bits]))
      |> Enum.reverse()
      |> Enum.reject(&(elem(&1, 0) in @elixir_internals))
    end
  end

  defp random_id() do
    :crypto.strong_rand_bytes(20) |> Base.encode32(case: :lower)
  end

  defp time_diff_ms(started_at) do
    System.monotonic_time()
    |> Kernel.-(started_at)
    |> System.convert_time_unit(:native, :millisecond)
  end

  @doc false
  def write_module!(module, bytecode) do
    if ebin_path = ebin_path() do
      ebin_path
      |> Path.join("#{module}.beam")
      |> File.write!(bytecode)
    end
  end

  @doc false
  def delete_module(module, ebin_path \\ ebin_path()) do
    # If there is a deleted code for the module, we purge it first
    :code.purge(module)

    :code.delete(module)

    if ebin_path do
      ebin_path
      |> Path.join("#{module}.beam")
      |> File.rm()
    end
  end

  defp ebin_path() do
    Process.get(@ebin_path_key)
  end
end

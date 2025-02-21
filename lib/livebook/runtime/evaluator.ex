defmodule Livebook.Runtime.Evaluator do
  # A process responsible for evaluating notebook code.
  #
  # When evaluator receives an evaluation request, it synchronously
  # evaluates the given code within itself, rather than spawning a
  # separate process. It stores the resulting binding and env in its
  # state (under a specific reference).
  #
  # Storing the binding in the same process that evaluates the code is
  # essential, because otherwise we would have to send it to another
  # process, which means copying potentially massive amounts of data.
  #
  # Also, note that this process intentionally is not a GenServer,
  # because during evaluation it may receive arbitrary messages
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
          client_tracker: pid(),
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

  # We store the path in the process dictionary, so that the tracer can access it.
  @ebin_path_key :ebin_path

  @doc """
  Starts an evaluator.

  ## Options

    * `:send_to` - the process to send evaluation messages to. Required

    * `:object_tracker` - a pid of `Livebook.Runtime.Evaluator.ObjectTracker`.
      Required

    * `:client_tracker` - a pid of `Livebook.Runtime.Evaluator.ClientTracker`.
      Required

    * `:runtime_broadcast_to` - the process to send runtime broadcast
      events to. Defaults to the value of `:send_to`

    * `:ebin_path` - a directory to write modules bytecode into. When
      not specified, modules are not written to disk

    * `:tmp_dir` - a temporary directory for arbitrary use during
      evaluation

  """
  @spec start_link(keyword()) :: {:ok, pid(), t()} | {:error, term()}
  def start_link(opts \\ []) do
    :proc_lib.start_link(__MODULE__, :init, [opts])
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
  @spec evaluate_code(t(), Livebook.Runtime.language(), ref(), list(ref()), keyword()) :: :ok
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
  @spec intellisense_context() :: Livebook.Intellisense.context()
  def intellisense_context() do
    env = Code.env_for_eval([])
    map_binding = fn fun -> fun.([]) end
    %{env: env, ebin_path: ebin_path(), map_binding: map_binding}
  end

  @doc """
  Builds intellisense context from the given evaluation.
  """
  @spec intellisense_context(t(), list(ref())) :: Livebook.Intellisense.context()
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

    %{
      env: env,
      ebin_path: find_in_dictionary(dictionary, @ebin_path_key),
      map_binding: map_binding
    }
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
    client_tracker = Keyword.fetch!(opts, :client_tracker)
    ebin_path = Keyword.get(opts, :ebin_path)
    tmp_dir = Keyword.get(opts, :tmp_dir)

    {:ok, io_proxy} =
      Evaluator.IOProxy.start(%{
        evaluator: self(),
        send_to: send_to,
        runtime_broadcast_to: runtime_broadcast_to,
        object_tracker: object_tracker,
        client_tracker: client_tracker,
        ebin_path: ebin_path,
        tmp_dir: tmp_dir
      })

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
      client_tracker: client_tracker,
      contexts: %{},
      initial_context: context,
      initial_context_version: nil,
      ignored_pdict_keys: ignored_pdict_keys,
      tmp_dir: tmp_dir
    }

    :proc_lib.init_ack({:ok, evaluator.pid, evaluator})

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
    %{id: random_long_id(), binding: [], env: env, pdict: %{}}
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

    if opts[:disable_dependencies_cache] do
      System.put_env("MIX_INSTALL_FORCE", "true")
    end

    start_time = System.monotonic_time()

    {eval_result, code_markers} =
      case language do
        :elixir -> eval_elixir(code, context.binding, context.env)
        :erlang -> eval_erlang(code, context.binding, context.env, state.tmp_dir)
        :python -> eval_python(code, context.binding, context.env)
        :"pyproject.toml" -> eval_pyproject_toml(code, context.binding, context.env)
      end

    evaluation_time_ms = time_diff_ms(start_time)

    %{tracer_info: tracer_info} = Evaluator.IOProxy.after_evaluation(state.io_proxy)

    {new_context, result, identifiers_used, identifiers_defined, identifier_definitions} =
      case eval_result do
        {:ok, value, binding, env} ->
          context_id = random_long_id()

          new_context = %{
            id: context_id,
            binding: binding,
            env: prune_env(env, tracer_info),
            pdict: current_pdict(state)
          }

          {identifiers_used, identifiers_defined} =
            identifier_dependencies(new_context, tracer_info, context)

          identifier_definitions = definitions(new_context, tracer_info)

          result = {:ok, value}
          {new_context, result, identifiers_used, identifiers_defined, identifier_definitions}

        {:error, kind, error, stacktrace} ->
          for {module, _} <- tracer_info.modules_defined do
            delete_module(module)
          end

          result = {:error, kind, error, stacktrace}
          identifiers_used = :unknown
          identifiers_defined = %{}
          identifier_definitions = []

          # Mostly empty context, however we keep imports and process
          # dictionary from the previous context, since these are not
          # diffed
          new_context = %{
            id: random_long_id(),
            binding: [],
            env:
              context.env
              |> prune_env(%Evaluator.Tracer{})
              |> Map.replace!(:versioned_vars, %{}),
            pdict: context.pdict
          }

          {new_context, result, identifiers_used, identifiers_defined, identifier_definitions}
      end

    if ebin_path() do
      Livebook.Runtime.Evaluator.Doctests.run(new_context.env.context_modules, code)
    end

    state = put_context(state, ref, new_context)
    output = Evaluator.Formatter.format_result(language, result)

    metadata = %{
      errored: error_result?(result),
      interrupted: interrupt_result?(result),
      evaluation_time_ms: evaluation_time_ms,
      memory_usage: memory(),
      code_markers: code_markers,
      identifiers_used: identifiers_used,
      identifiers_defined: identifiers_defined,
      identifier_definitions: identifier_definitions
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
    %{id: random_long_id(), binding: binding, env: env, pdict: pdict}
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

  defp eval_elixir(code, binding, env) do
    {{result, extra_diagnostics}, diagnostics} =
      Code.with_diagnostics([log: true], fn ->
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

  defp extra_diagnostic?(%SyntaxError{}), do: true
  defp extra_diagnostic?(%TokenMissingError{}), do: true
  defp extra_diagnostic?(%MismatchedDelimiterError{}), do: true

  defp extra_diagnostic?(%CompileError{description: description}) do
    not String.contains?(description, "(errors have been logged)")
  end

  defp extra_diagnostic?(_error), do: false

  # Erlang code is either statements as currently supported, or modules.
  # In case we want to support modules - it makes sense to allow users to use
  # includes, defines and thus we use the epp-module first - try to find out
  #
  # if in the tokens from erl_scan we find at least 1 module-token we assume
  # that the user is defining a module, if not the previous code is called.

  defp eval_erlang(code, binding, env, tmp_dir) do
    case :erl_scan.string(String.to_charlist(code), {1, 1}, [:text]) do
      {:ok, [{:-, _}, {:atom, _, :module} | _], _} ->
        eval_erlang_module(code, binding, env, tmp_dir)

      {:ok, tokens, _} ->
        eval_erlang_statements(code, tokens, binding, env)

      {:error, {location, module, description}, _end_loc} ->
        process_erlang_error(env, code, location, module, description)
    end
  end

  # Explain to user: without tmp_dir to write files, they cannot compile erlang-modules
  defp eval_erlang_module(_code, _binding, _env, nil) do
    {{:error, :error, "writing Erlang modules requires a writeable file system", []}, []}
  end

  defp eval_erlang_module(code, binding, env, tmp_dir) do
    # Consider using in-memory file, once :ram file supports IO device API.
    # See https://github.com/erlang/otp/issues/7239
    filename = Path.join(tmp_dir, "epp.tmp")
    File.mkdir_p!(tmp_dir)
    File.write!(filename, code)

    try do
      {:ok, forms} = :epp.parse_file(filename, source_name: String.to_charlist(env.file))

      case :compile.forms(forms) do
        {:ok, module, binary} ->
          file =
            if ebin_path = ebin_path() do
              Path.join(ebin_path, "#{module}.beam")
            else
              "#{module}.beam"
            end

          {:module, module} =
            :code.load_binary(module, String.to_charlist(file), binary)

          # Registration of module
          Evaluator.Tracer.trace(
            {:on_module, binary, %{}},
            %{env | module: module, versioned_vars: %{}}
          )

          {{:ok, {:ok, module}, binding, env}, []}

        # TODO: deal with errors and reports as diagnostics
        :error ->
          {{:error, :error, "compile forms error", []}, []}
      end
    catch
      kind, error ->
        stacktrace = prune_stacktrace(:erl_eval, __STACKTRACE__)
        {{:error, kind, error, stacktrace}, []}
    after
      # Clean up after ourselves.
      _ = File.rm(filename)
    end
  end

  defp eval_erlang_statements(code, tokens, binding, env) do
    try do
      erl_binding =
        Enum.reduce(binding, %{}, fn {name, value}, erl_binding ->
          :erl_eval.add_binding(elixir_to_erlang_var(name), value, erl_binding)
        end)

      with {:ok, parsed} <- :erl_parse.parse_exprs(tokens),
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
    if :erl_anno.column(location) != :undefined and :string.trim(code, :leading) != [] do
      line = :erl_anno.line(location)
      lines = :string.split(code, "\n", :all)
      :lists.nth(line, lines)
    end
  end

  defp elixir_to_erlang_var(name) do
    name
    |> :erlang.atom_to_binary()
    |> toggle_var_case()
    |> :erlang.binary_to_atom()
  end

  defp erlang_to_elixir_var(name) do
    name
    |> :erlang.atom_to_binary()
    |> toggle_var_case()
    |> :erlang.binary_to_atom()
  end

  # Unambiguously maps variable names from camel case to underscore
  # case, and vice-versa. The mapping is defined as follows:
  #
  #   1. The first character case is changed
  #
  #   2. Underscore followed by lower character maps to upper character,
  #      and vice-versa
  #
  defp toggle_var_case(<<h, t::binary>>) do
    do_toggle_var_case(<<toggle_char_case(h)>>, t)
  end

  defp do_toggle_var_case(acc, <<?_, h, t::binary>>) when h in ?a..?z do
    do_toggle_var_case(<<acc::binary, toggle_char_case(h)>>, t)
  end

  defp do_toggle_var_case(acc, <<h, t::binary>>) when h in ?A..?Z do
    do_toggle_var_case(<<acc::binary, ?_, toggle_char_case(h)>>, t)
  end

  defp do_toggle_var_case(acc, <<h, t::binary>>) do
    do_toggle_var_case(<<acc::binary, h>>, t)
  end

  defp do_toggle_var_case(acc, <<>>), do: acc

  defp toggle_char_case(char) when char in ?a..?z, do: char - 32
  defp toggle_char_case(char) when char in ?A..?Z, do: char + 32
  defp toggle_char_case(char), do: char

  defp filter_erlang_code_markers(code_markers) do
    Enum.reject(code_markers, &(&1.line == 0))
  end

  @compile {:no_warn_undefined, {Pythonx, :eval, 2}}
  @compile {:no_warn_undefined, {Pythonx, :decode, 1}}

  defp eval_python(code, binding, env) do
    with :ok <- ensure_pythonx() do
      {result, _diagnostics} =
        Code.with_diagnostics([log: true], fn ->
          try do
            quoted = python_code_to_quoted(code)

            {value, binding, env} =
              Code.eval_quoted_with_env(quoted, binding, env, prune_binding: true)

            result = {:ok, value, binding, env}
            code_markers = []
            {result, code_markers}
          catch
            kind, error ->
              code_markers =
                if is_struct(error, Pythonx.Error) do
                  Pythonx.eval(
                    """
                    import traceback

                    if traceback_ is None:
                      diagnostic = None
                    elif isinstance(value, SyntaxError):
                      diagnostic = (value.lineno, "SyntaxError: invalid syntax")
                    else:
                      description = " ".join(traceback.format_exception_only(type, value)).strip()
                      diagnostic = (traceback_.tb_lineno, description)

                    diagnostic
                    """,
                    %{
                      "type" => error.type,
                      "value" => error.value,
                      "traceback_" => error.traceback
                    }
                  )
                  |> elem(0)
                  |> Pythonx.decode()
                  |> case do
                    nil -> []
                    {line, message} -> [%{line: line, description: message, severity: :error}]
                  end
                else
                  []
                end

              result = {:error, kind, error, []}
              {result, code_markers}
          end
        end)

      result
    end
  end

  defp python_code_to_quoted(code) do
    # We expand the sigil upfront, so it is not traced as import usage
    # during evaluation.

    quoted = {:sigil_PY, [], [{:<<>>, [], [code]}, []]}

    env = Code.env_for_eval([])

    env =
      env
      |> Map.replace!(:requires, [Pythonx])
      |> Map.replace!(:macros, [{Pythonx, [{:sigil_PY, 2}]}])

    ast = Macro.expand_once(quoted, env)

    # We modify the Pythonx.eval/2 call to specify the :stderr_device
    # option. We want to Python stderr output to also be send to our
    # group leader. By default it would be sent to our :standard_error,
    # which sends it further to sender's group leader, however the
    # sender is a process in the Pythonx supervision tree and has the
    # default group leader.mix
    Macro.prewalk(ast, fn
      {{:., _, [{:__aliases__, _, [:Pythonx]}, :eval]} = target, meta, [code, globals]} ->
        opts = [
          stderr_device: {{:., [], [{:__aliases__, [], [:Process]}, :group_leader]}, [], []}
        ]

        {target, meta, [code, globals, opts]}

      other ->
        other
    end)
  end

  defp eval_pyproject_toml(code, binding, env) do
    with :ok <- ensure_pythonx() do
      quoted = {{:., [], [{:__aliases__, [alias: false], [:Pythonx]}, :uv_init]}, [], [code]}

      {result, _diagnostics} =
        Code.with_diagnostics([log: true], fn ->
          try do
            {value, binding, env} =
              Code.eval_quoted_with_env(quoted, binding, env, prune_binding: true)

            result = {:ok, value, binding, env}
            code_markers = []
            {result, code_markers}
          catch
            kind, error ->
              code_markers = []

              result = {:error, kind, error, []}
              {result, code_markers}
          end
        end)

      result
    end
  end

  defp ensure_pythonx() do
    pythonx_requirement = Livebook.Runtime.Definitions.pythonx_requirement()

    cond do
      not Code.ensure_loaded?(Pythonx) ->
        message =
          """
          Pythonx is missing, make sure to add it as a dependency:

              #{Macro.to_string(Livebook.Runtime.Definitions.pythonx_dependency().dep)}
          """

        exception = RuntimeError.exception(message)
        {{:error, :error, exception, []}, []}

      not Version.match?(pythonx_version(), pythonx_requirement) ->
        message =
          "this Livebook version requires Pythonx #{pythonx_requirement}," <>
            " but #{pythonx_version()} is installed, please update the dependency"

        exception = RuntimeError.exception(message)
        {{:error, :error, exception, []}, []}

      true ->
        :ok
    end
  end

  defp pythonx_version(), do: List.to_string(Application.spec(:pythonx)[:vsn])

  defp identifier_dependencies(context, tracer_info, prev_context) do
    identifiers_used = MapSet.new()
    identifiers_defined = %{}

    # Variables

    identifiers_used =
      for var <- vars_used(context, tracer_info, prev_context),
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

    # Note: `module_info` works for both Erlang and Elixir modules, as opposed to `__info__`
    identifiers_defined =
      for {module, _line_vars} <- tracer_info.modules_defined, into: identifiers_defined do
        {{:module, module}, module.module_info(:md5)}
      end

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
      for {_module, {_line, vars}} <- tracer_info.modules_defined,
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

  # See https://github.com/elixir-lang/elixir/blob/792d4cc6310c56eb9772056a6b5fb3339ce17b0f/lib/iex/lib/iex/evaluator.ex#L436-L445
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

  defp random_long_id() do
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

  defp definitions(context, tracer_info) do
    for {module, {line, _vars}} <- tracer_info.modules_defined,
        do: %{label: module_name(module), file: context.env.file, line: line}
  end

  defp module_name(module) do
    case Atom.to_string(module) do
      "Elixir." <> name -> name
      name -> name
    end
  end
end

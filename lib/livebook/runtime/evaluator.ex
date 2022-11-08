defmodule Livebook.Runtime.Evaluator do
  @moduledoc false

  # A process responsible for evaluating notebook code.
  #
  # Evaluator receives an evaluation request and synchronously
  # evaluates the given code within itself (rather than spawning
  # a separate process). It stores the resulting binding and env
  # in its state (under a specific reference).
  #
  # Storing the binding in the same process that evaluates the
  # code is essential, because otherwise we would have to send it
  # to another process, which means copying a potentially massive
  # amounts of data.
  #
  # Also, note that this process is intentionally not a GenServer,
  # because during evaluation we it may receive arbitrary messages
  # and we want to keep them in the inbox, while a GenServer would
  # always consume them.

  require Logger

  alias Livebook.Runtime.Evaluator

  @type t :: %{pid: pid(), ref: reference()}

  @type state :: %{
          evaluator_ref: reference(),
          formatter: module(),
          io_proxy: pid(),
          send_to: pid(),
          runtime_broadcast_to: pid(),
          object_tracker: pid(),
          contexts: %{ref() => context()},
          initial_context: context()
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

  @doc """
  Starts an evaluator.

  ## Options

    * `:send_to` - the process to send evaluation messages to. Required

    * `:object_tracker` - a pid of `Livebook.Runtime.Evaluator.ObjectTracker`.
      Required

    * `:runtime_broadcast_to` - the process to send runtime broadcast
      events to. Defaults to the value of `:send_to`

    * `:formatter` - a module implementing the `Livebook.Runtime.Evaluator.Formatter`
      behaviour, used for transforming evaluation result before sending
      it to the client. Defaults to identity
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

  Any exceptions are captured and transformed into an error
  result.

  The resulting context (binding and env) is stored under `ref`. Any
  subsequent calls may specify `parent_refs` pointing to a sequence
  of previous evaluations, in which case the corresponding context is
  used as the entry point for evaluation.

  The evaluation result is transformed with the configured
  formatter send to the configured client (see `start_link/1`).

  See `Livebook.Runtime.evaluate_code/5` for the messages format
  and the list of available options.

  ## Options

    * `:on_finish` - a function to run when the evaluation is
      finished. The function receives `t:evaluation_result/0`
      as an argument
  """
  @spec evaluate_code(t(), String.t(), ref(), list(ref()), keyword()) :: :ok
  def evaluate_code(evaluator, code, ref, parent_refs, opts \\ []) do
    cast(evaluator, {:evaluate_code, code, ref, parent_refs, opts})
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
    formatter = Keyword.get(opts, :formatter, Evaluator.IdentityFormatter)

    {:ok, io_proxy} =
      Evaluator.IOProxy.start_link(self(), send_to, runtime_broadcast_to, object_tracker)

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

    ignored_pdict_keys = Process.get_keys() |> MapSet.new()

    state = %{
      evaluator_ref: evaluator_ref,
      formatter: formatter,
      io_proxy: io_proxy,
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
    end
  end

  defp initial_context() do
    env = Code.env_for_eval([])
    env = Macro.Env.prepend_tracer(env, Evaluator.Tracer)
    %{id: random_id(), binding: [], env: env, pdict: %{}}
  end

  defp handle_cast({:evaluate_code, code, ref, parent_refs, opts}, state) do
    Evaluator.ObjectTracker.remove_reference_sync(state.object_tracker, {self(), ref})

    context = get_context(state, parent_refs)
    file = Keyword.get(opts, :file, "nofile")
    context = put_in(context.env.file, file)

    Evaluator.IOProxy.configure(state.io_proxy, ref, file)

    set_pdict(context, state.ignored_pdict_keys)

    start_time = System.monotonic_time()
    eval_result = eval(code, context.binding, context.env)
    evaluation_time_ms = time_diff_ms(start_time)

    {result_context, result, code_error, identifiers_used, identifiers_defined} =
      case eval_result do
        {:ok, value, binding, env} ->
          tracer_info = Evaluator.IOProxy.get_tracer_info(state.io_proxy)
          context_id = random_id()

          result_context = %{
            id: context_id,
            binding: binding,
            env: prune_env(env, tracer_info),
            pdict: current_pdict(state)
          }

          {identifiers_used, identifiers_defined} =
            identifier_dependencies(result_context, tracer_info, context)

          result = {:ok, value}
          {result_context, result, nil, identifiers_used, identifiers_defined}

        {:error, kind, error, stacktrace, code_error} ->
          result = {:error, kind, error, stacktrace}
          identifiers_used = :unknown
          identifiers_defined = %{}
          # Empty context
          result_context = initial_context()
          {result_context, result, code_error, identifiers_used, identifiers_defined}
      end

    state = put_context(state, ref, result_context)

    Evaluator.IOProxy.flush(state.io_proxy)
    Evaluator.IOProxy.clear_input_cache(state.io_proxy)

    output = state.formatter.format_result(result)

    metadata = %{
      evaluation_time_ms: evaluation_time_ms,
      memory_usage: memory(),
      code_error: code_error,
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

  defp handle_cast({:forget_evaluation, ref}, state) do
    state = delete_context(state, ref)
    Evaluator.ObjectTracker.remove_reference_sync(state.object_tracker, {self(), ref})

    :erlang.garbage_collect(self())
    {:noreply, state}
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

  defp put_context(state, ref, context) do
    update_evaluator_info(fn info ->
      put_in(info.contexts[ref], {context.id, context.env})
    end)

    put_in(state.contexts[ref], context)
  end

  defp delete_context(state, ref) do
    update_evaluator_info(fn info ->
      {_, info} = pop_in(info.contexts[ref])
      info
    end)

    {_, state} = pop_in(state.contexts[ref])
    state
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
    |> Map.replace!(:requires, MapSet.to_list(tracer_info.requires_defined))
  end

  defp merge_context(prev_context, context) do
    binding = merge_binding(prev_context.binding, context.binding)
    env = merge_env(prev_context.env, context.env)
    pdict = context.pdict
    %{id: random_id(), binding: binding, env: env, pdict: pdict}
  end

  defp merge_binding(prev_binding, binding) do
    new_keys = MapSet.new(binding, &elem(&1, 0))
    kept_binding = Enum.reject(prev_binding, &(elem(&1, 0) in new_keys))
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
    |> Map.update!(:requires, fn requires ->
      (prev_env.requires ++ requires)
      |> Enum.sort()
      |> Enum.dedup()
    end)
    |> Map.replace!(:context_modules, [])
  end

  @compile {:no_warn_undefined, {Code, :eval_quoted_with_env, 4}}

  defp eval(code, binding, env) do
    try do
      quoted = Code.string_to_quoted!(code, file: env.file)

      # TODO: remove the else branch when we require Elixir v1.14.2
      {value, binding, env} =
        if function_exported?(Code, :eval_quoted_with_env, 4) do
          Code.eval_quoted_with_env(quoted, binding, env, prune_binding: true)
        else
          Code.eval_quoted_with_env(quoted, binding, env)
        end

      {:ok, value, binding, env}
    catch
      kind, error ->
        stacktrace = prune_stacktrace(__STACKTRACE__)

        code_error =
          if code_error?(error) and (error.file == env.file and error.file != "nofile") do
            %{line: error.line, description: error.description}
          else
            nil
          end

        {:error, kind, error, stacktrace, code_error}
    end
  end

  defp code_error?(%SyntaxError{}), do: true
  defp code_error?(%TokenMissingError{}), do: true
  defp code_error?(%CompileError{}), do: true
  defp code_error?(_error), do: false

  defp identifier_dependencies(context, tracer_info, prev_context) do
    identifiers_used = MapSet.new()
    identifiers_defined = %{}

    # Variables

    identifiers_used =
      for var_name <- vars_used(context, tracer_info, prev_context),
          do: {:variable, var_name},
          into: identifiers_used

    identifiers_used =
      for var_name <- tracer_info.undefined_vars,
          do: {:variable, var_name},
          into: identifiers_used

    identifiers_defined =
      for var_name <- vars_defined(context, prev_context),
          do: {{:variable, var_name}, context.id},
          into: identifiers_defined

    # Modules

    identifiers_used =
      for module <- tracer_info.modules_used,
          do: {:module, module},
          into: identifiers_used

    identifiers_defined =
      for {module, {version, _vars}} <- tracer_info.modules_defined,
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
        version = :erlang.phash2({context.env.functions, context.env.macros})
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
      for {{name, nil}, _version} <- prev_context.env.versioned_vars,
          into: MapSet.new(),
          do: name

    outer_used_vars =
      for {{name, nil}, _version} <- context.env.versioned_vars,
          into: MapSet.new(),
          do: name

    # Note that :prune_binding removes variables used by modules
    # (unless used outside), so we get those from the tracer
    module_used_vars =
      for {_module, {_version, vars}} <- tracer_info.modules_defined,
          {name, nil} <- vars,
          into: MapSet.new(),
          do: name

    # We take an intersection with previous vars, so we ignore variables
    # that we know are newly defined
    MapSet.intersection(prev_vars, MapSet.union(outer_used_vars, module_used_vars))
  end

  defp vars_defined(context, prev_context) do
    for {{var, nil}, version} <- context.env.versioned_vars,
        version >= map_size(prev_context.env.versioned_vars),
        into: MapSet.new(),
        do: var
  end

  # Adapted from https://github.com/elixir-lang/elixir/blob/1c1654c88adfdbef38ff07fc30f6fbd34a542c07/lib/iex/lib/iex/evaluator.ex#L355-L372
  # TODO: Remove else branch once we depend on the versions below
  if System.otp_release() >= "25" do
    defp prune_stacktrace(stack) do
      stack
      |> Enum.reverse()
      |> Enum.drop_while(&(elem(&1, 0) != :elixir_eval))
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

    defp prune_stacktrace(stacktrace) do
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
end

defmodule Livebook.Evaluator do
  @moduledoc false

  # A process responsible for evaluating notebook code.
  #
  # The process receives evaluation request and synchronously
  # evaluates the given code within itself (rather than spawning a separate process).
  # It stores the resulting binding and env as part of the state.
  #
  # It's important to store the binding in the same process
  # where the evaluation happens, as otherwise we would have to
  # send them between processes, effectively copying potentially large data.
  #
  # Note that this process is intentionally not a GenServer,
  # because we during evaluation we may receive arbitrary
  # messages and we don't want to consume them from the inbox,
  # as GenServer does.

  require Logger

  alias Livebook.Evaluator

  @type t :: %{pid: pid(), ref: reference()}

  @type state :: %{
          ref: reference(),
          formatter: module(),
          io_proxy: pid(),
          object_tracker: pid(),
          contexts: %{ref() => context()},
          initial_context: context()
        }

  @typedoc """
  An evaluation context.
  """
  @type context :: %{binding: Code.binding(), env: Macro.Env.t(), id: binary()}

  @typedoc """
  A term used to identify evaluation.
  """
  @type ref :: term()

  @typedoc """
  Either {:ok, result} for successful evaluation
  or {:error, kind, error, stacktrace} for a failed one.
  """
  @type evaluation_response ::
          {:ok, any()} | {:error, Exception.kind(), any(), Exception.stacktrace()}

  # We store evaluation envs in process dictionary, so that we can
  # build intellisense context without asking the evaluator
  @env_key :evaluation_env
  @initial_env_key :initial_env

  ## API

  @doc """
  Starts the evaluator.

  Options:

    * `object_tracker` - a PID of `Livebook.Evaluator.ObjectTracker`, required

    * `formatter` - a module implementing the `Livebook.Evaluator.Formatter` behaviour,
      used for transforming evaluation response before it's sent to the client
  """
  @spec start_link(keyword()) :: {:ok, pid(), t()} | {:error, term()}
  def start_link(opts \\ []) do
    case :proc_lib.start_link(__MODULE__, :init, [opts]) do
      {:error, error} -> {:error, error}
      evaluator -> {:ok, evaluator.pid, evaluator}
    end
  end

  @doc """
  Computes the memory usage from this evaluator node.
  """
  @spec memory :: Livebook.Runtime.runtime_memory()
  def memory do
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

  Any exceptions are captured, in which case this method returns an error.

  The evaluator stores the resulting binding and environment under `ref`.
  Any subsequent calls may specify `prev_ref` pointing to a previous evaluation,
  in which case the corresponding binding and environment are used during evaluation.

  Evaluation response is sent to the process identified by `send_to` as `{:evaluation_response, ref, response, metadata}`.
  Note that response is transformed with the configured formatter (identity by default).

  ## Options

    * `:file` - file to which the evaluated code belongs. Most importantly,
      this has an impact on the value of `__DIR__`.

  """
  @spec evaluate_code(t(), pid(), String.t(), ref(), ref() | nil, keyword()) :: :ok
  def evaluate_code(evaluator, send_to, code, ref, prev_ref \\ nil, opts \\ []) when ref != nil do
    cast(evaluator, {:evaluate_code, send_to, code, ref, prev_ref, opts})
  end

  @doc """
  Fetches evaluation context (binding and environment) by evaluation reference.

  ## Options

    * `cached_id` - id of context that the sender may already have,
      if it matches the fetched context the `{:error, :not_modified}`
      tuple is returned instead
  """
  @spec fetch_evaluation_context(t(), ref(), keyword()) ::
          {:ok, context()} | {:error, :not_modified}
  def fetch_evaluation_context(evaluator, ref, opts \\ []) do
    cached_id = opts[:cached_id]

    call(evaluator, {:fetch_evaluation_context, ref, cached_id})
  end

  @doc """
  Fetches an evaluation context from another `Evaluator` process
  and configures it as the initial context for this evaluator.

  The process dictionary is also copied to match the given evaluator.
  """
  @spec initialize_from(t(), t(), ref()) :: :ok
  def initialize_from(evaluator, source_evaluator, source_evaluation_ref) do
    call(evaluator, {:initialize_from, source_evaluator, source_evaluation_ref})
  end

  @doc """
  Removes the evaluation identified by `ref` from history,
  so that further evaluations cannot use it.
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
    # TODO: Use Code.env_for_eval and eval_quoted_with_env on Elixir v1.14+
    env = :elixir.env_for_eval([])
    map_binding = fn fun -> fun.([]) end
    %{env: env, map_binding: map_binding}
  end

  @doc """
  Builds intellisense context from the given evaluation.
  """
  @spec intellisense_context(t(), ref()) :: Livebook.Intellisense.intellisense_context()
  def intellisense_context(evaluator, ref) do
    {:dictionary, dictionary} = Process.info(evaluator.pid, :dictionary)

    env =
      find_in_dictionary(dictionary, {@env_key, ref}) ||
        find_in_dictionary(dictionary, @initial_env_key)

    map_binding = fn fun -> map_binding(evaluator, ref, fun) end

    %{env: env, map_binding: map_binding}
  end

  defp find_in_dictionary(dictionary, key) do
    Enum.find_value(dictionary, fn
      {^key, value} -> value
      _pair -> nil
    end)
  end

  # Applies the given function to evaluation binding
  defp map_binding(evaluator, ref, fun) do
    call(evaluator, {:map_binding, ref, fun})
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

  ## Callbacks

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :temporary
    }
  end

  def init(opts) do
    object_tracker = Keyword.fetch!(opts, :object_tracker)
    formatter = Keyword.get(opts, :formatter, Evaluator.IdentityFormatter)

    {:ok, io_proxy} = Evaluator.IOProxy.start_link(self(), object_tracker)

    # Use the dedicated IO device as the group leader, so that
    # intercepts all :stdio requests and also handles Livebook
    # specific ones
    Process.group_leader(self(), io_proxy)

    evaluator_ref = make_ref()
    state = initial_state(evaluator_ref, formatter, io_proxy, object_tracker)
    evaluator = %{pid: self(), ref: evaluator_ref}

    :proc_lib.init_ack(evaluator)

    loop(state)
  end

  defp initial_state(evaluator_ref, formatter, io_proxy, object_tracker) do
    context = initial_context()
    Process.put(@initial_env_key, context.env)

    %{
      evaluator_ref: evaluator_ref,
      formatter: formatter,
      io_proxy: io_proxy,
      object_tracker: object_tracker,
      contexts: %{},
      initial_context: context
    }
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
    # TODO: Use Code.env_for_eval and eval_quoted_with_env on Elixir v1.14+
    env = :elixir.env_for_eval([])
    %{binding: [], env: env, id: random_id()}
  end

  defp handle_cast({:evaluate_code, send_to, code, ref, prev_ref, opts}, state) do
    Evaluator.IOProxy.configure(state.io_proxy, send_to, ref)

    Evaluator.ObjectTracker.remove_reference(state.object_tracker, {self(), ref})

    context = get_context(state, prev_ref)
    file = Keyword.get(opts, :file, "nofile")
    context = put_in(context.env.file, file)
    start_time = System.monotonic_time()

    {result_context, response, code_error} =
      case eval(code, context.binding, context.env) do
        {:ok, result, binding, env} ->
          result_context = %{binding: binding, env: env, id: random_id()}
          response = {:ok, result}
          {result_context, response, nil}

        {:error, kind, error, stacktrace, code_error} ->
          response = {:error, kind, error, stacktrace}
          {context, response, code_error}
      end

    evaluation_time_ms = get_execution_time_delta(start_time)

    state = put_context(state, ref, result_context)

    Evaluator.IOProxy.flush(state.io_proxy)
    Evaluator.IOProxy.clear_input_cache(state.io_proxy)

    output = state.formatter.format_response(response)

    metadata = %{
      evaluation_time_ms: evaluation_time_ms,
      memory_usage: memory(),
      code_error: code_error
    }

    send(send_to, {:evaluation_response, ref, output, metadata})

    :erlang.garbage_collect(self())
    {:noreply, state}
  end

  defp handle_cast({:forget_evaluation, ref}, state) do
    state = delete_context(state, ref)
    Evaluator.ObjectTracker.remove_reference(state.object_tracker, {self(), ref})

    :erlang.garbage_collect(self())
    {:noreply, state}
  end

  defp handle_call({:fetch_evaluation_context, ref, cached_id}, _from, state) do
    context = get_context(state, ref)

    reply =
      if context.id == cached_id do
        {:error, :not_modified}
      else
        {:ok, context}
      end

    {:reply, reply, state}
  end

  defp handle_call({:initialize_from, source_evaluator, source_evaluation_ref}, _from, state) do
    state =
      case Evaluator.fetch_evaluation_context(
             source_evaluator,
             source_evaluation_ref,
             cached_id: state.initial_context.id
           ) do
        {:ok, context} ->
          # If the context changed, mirror the process dictionary again
          copy_process_dictionary_from(source_evaluator)

          Process.put(@initial_env_key, context.env)
          put_in(state.initial_context, context)

        {:error, :not_modified} ->
          state
      end

    {:reply, :ok, state}
  end

  defp handle_call({:map_binding, ref, fun}, _from, state) do
    context = get_context(state, ref)
    result = fun.(context.binding)
    {:reply, result, state}
  end

  defp put_context(state, ref, context) do
    Process.put({@env_key, ref}, context.env)
    put_in(state.contexts[ref], context)
  end

  defp delete_context(state, ref) do
    Process.delete({@env_key, ref})
    {_, state} = pop_in(state.contexts[ref])
    state
  end

  defp get_context(state, ref) do
    Map.get_lazy(state.contexts, ref, fn -> state.initial_context end)
  end

  defp eval(code, binding, env) do
    try do
      quoted = Code.string_to_quoted!(code, file: env.file)
      # TODO: Use Code.eval_quoted_with_env/3 on Elixir v1.14
      {result, binding, env} = :elixir.eval_quoted(quoted, binding, env)
      # TODO: Remove this line on Elixir v1.14 as binding propagates to env correctly
      {_, binding, env} = :elixir.eval_forms(:ok, binding, env)

      {:ok, result, binding, env}
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

  # Adapted from https://github.com/elixir-lang/elixir/blob/1c1654c88adfdbef38ff07fc30f6fbd34a542c07/lib/iex/lib/iex/evaluator.ex#L355-L372

  @elixir_internals [:elixir, :elixir_expand, :elixir_compiler, :elixir_module] ++
                      [:elixir_clauses, :elixir_lexical, :elixir_def, :elixir_map] ++
                      [:elixir_erl, :elixir_erl_clauses, :elixir_erl_pass]

  defp prune_stacktrace(stacktrace) do
    # The order in which each drop_while is listed is important.
    # For example, the user may call Code.eval_string/2 in their code
    # and if there is an error we should not remove erl_eval
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

  defp random_id() do
    :crypto.strong_rand_bytes(20) |> Base.encode32(case: :lower)
  end

  defp copy_process_dictionary_from(source_evaluator) do
    {:dictionary, dictionary} = Process.info(source_evaluator.pid, :dictionary)

    for {key, value} <- dictionary, not internal_dictionary_key?(key) do
      Process.put(key, value)
    end
  end

  defp internal_dictionary_key?("$" <> _), do: true
  defp internal_dictionary_key?({@env_key, _ref}), do: true
  defp internal_dictionary_key?(@initial_env_key), do: true
  defp internal_dictionary_key?(_), do: false

  defp get_execution_time_delta(started_at) do
    System.monotonic_time()
    |> Kernel.-(started_at)
    |> System.convert_time_unit(:native, :millisecond)
  end
end

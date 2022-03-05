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

  Each evaluation produces a new context, which may be optionally
  used by a later evaluation.
  """
  @type context :: %{binding: Code.binding(), env: Macro.Env.t(), id: binary()}

  @typedoc """
  A term used to identify evaluation.
  """
  @type ref :: term()

  @typedoc """
  An evaluation response, either the resulting value or an error
  if raised.
  """
  @type evaluation_response ::
          {:ok, result :: any()}
          | {:error, Exception.kind(), error :: any(), Exception.stacktrace()}

  # We store evaluation envs in the process dictionary, so that we
  # can build intellisense context without asking the evaluator
  @env_key :evaluation_env
  @initial_env_key :initial_env

  @doc """
  Starts an evaluator.

  ## Options

    * `:send_to` - the process to send evaluation messages to. Required

    * `:object_tracker` - a pid of `Livebook.Runtime.Evaluator.ObjectTracker`.
      Required

    * `:runtime_broadcast_to` - the process to send runtime broadcast
      events to. Defaults to the value of `:send_to`

    * `:formatter` - a module implementing the `Livebook.Runtime.Evaluator.Formatter`
      behaviour, used for transforming evaluation response before sending
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
  response.

  The resulting contxt (binding and env) is stored under `ref`.
  Any subsequent calls may specify `base_ref` pointing to a
  previous evaluation, in which case the corresponding context
  is used as the entry point for evaluation.

  The evaluation response is transformed with the configured
  formatter send to the configured client (see `start_link/1`).

  See `Livebook.Runtime.evaluate_code/5` for the messages format
  and the list of available options.

  ## Options

    * `:notify_to` - a process to be notified about finished
      evaluation. The notification is sent as a message of the
      form `{:evaluation_finished, pid, ref}`
  """
  @spec evaluate_code(t(), String.t(), ref(), ref() | nil, keyword()) :: :ok
  def evaluate_code(evaluator, code, ref, base_ref \\ nil, opts \\ []) when ref != nil do
    cast(evaluator, {:evaluate_code, code, ref, base_ref, opts})
  end

  @doc """
  Fetches the evaluation context (binding and env) for the given
  evaluation reference.

  ## Options

    * `:cached_id` - id of context that the sender may already have,
      if it matches the fetched context, `{:error, :not_modified}`
      is returned instead
  """
  @spec fetch_evaluation_context(t(), ref(), keyword()) ::
          {:ok, context()} | {:error, :not_modified}
  def fetch_evaluation_context(evaluator, ref, opts \\ []) do
    cached_id = opts[:cached_id]
    call(evaluator, {:fetch_evaluation_context, ref, cached_id})
  end

  @doc """
  Fetches an evaluation context from `source_evaluator` and configures
  it as the initial context for `evaluator`.

  The process dictionary is also copied to match `source_evaluator`.
  """
  @spec initialize_from(t(), t(), ref()) :: :ok
  def initialize_from(evaluator, source_evaluator, source_evaluation_ref) do
    call(evaluator, {:initialize_from, source_evaluator, source_evaluation_ref})
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

  @doc """
  Runs the given function with binding and env of the given evaluation.

  Ths function runs within the evaluator process, so that no data
  is copied between processes, unless explicitly sent.
  """
  @spec peek_context(t(), ref(), (context() -> any())) :: :ok
  def peek_context(evaluator, ref, fun) do
    cast(evaluator, {:peek_context, ref, fun})
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
    Process.put(@initial_env_key, context.env)

    state = %{
      evaluator_ref: evaluator_ref,
      formatter: formatter,
      io_proxy: io_proxy,
      send_to: send_to,
      runtime_broadcast_to: runtime_broadcast_to,
      object_tracker: object_tracker,
      contexts: %{},
      initial_context: context
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
    # TODO: Use Code.env_for_eval and eval_quoted_with_env on Elixir v1.14+
    env = :elixir.env_for_eval([])
    %{binding: [], env: env, id: random_id()}
  end

  defp handle_cast({:evaluate_code, code, ref, base_ref, opts}, state) do
    Evaluator.IOProxy.configure(state.io_proxy, ref)

    Evaluator.ObjectTracker.remove_reference(state.object_tracker, {self(), ref})

    context = get_context(state, base_ref)
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

    send(state.send_to, {:runtime_evaluation_response, ref, output, metadata})

    if notify_to = opts[:notify_to] do
      send(notify_to, {:evaluation_finished, self(), ref})
    end

    :erlang.garbage_collect(self())
    {:noreply, state}
  end

  defp handle_cast({:forget_evaluation, ref}, state) do
    state = delete_context(state, ref)
    Evaluator.ObjectTracker.remove_reference(state.object_tracker, {self(), ref})

    :erlang.garbage_collect(self())
    {:noreply, state}
  end

  defp handle_cast({:peek_context, ref, fun}, state) do
    context = get_context(state, ref)
    fun.(context)
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
  # TODO: Remove else branch once we depend on the versions below
  if System.otp_release() >= "25" and Version.match?(System.version(), "~> 1.14-dev") do
    defp prune_stacktrace(stack) do
      stack
      |> Enum.reverse()
      |> Enum.drop_while(&(elem(&1, 0) != :elixir_eval))
      |> Enum.reverse()
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

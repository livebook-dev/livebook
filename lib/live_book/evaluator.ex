defmodule LiveBook.Evaluator do
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

  use GenServer, restart: :temporary

  alias LiveBook.Evaluator

  @type t :: GenServer.server()

  @type state :: %{
          io_proxy: pid(),
          contexts: %{ref() => context()}
        }

  @typedoc """
  An evaluation context.
  """
  @type context :: %{binding: Code.binding(), env: Macro.Env.t()}

  @typedoc """
  A term used to identify evaluation.
  """
  @type ref :: term()

  @typedoc """
  Either {:ok, result} for successfull evaluation
  or {:error, kind, error, stacktrace} for a failed one.
  """
  @type evaluation_response ::
          {:ok, any()} | {:error, Exception.kind(), any(), Exception.stacktrace()}

  ## API

  @doc """
  Starts the evaluator.

  Options:

  * `formatter` - a module implementing the `LiveBook.Evaluator.Formatter` behaviour,
    used for transforming evaluation response before it's sent to the client
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Asynchronously parses and evaluates the given code.

  Any exceptions are captured, in which case this method returns an error.

  The evaluator stores the resulting binding and environment under `ref`.
  Any subsequent calls may specify `prev_ref` pointing to a previous evaluation,
  in which case the corresponding binding and environment are used during evaluation.

  Evaluation response is sent to the process identified by `send_to` as `{:evaluation_response, ref, response}`.
  Note that response is transformed with the configured formatter (identity by default).
  """
  @spec evaluate_code(t(), pid(), String.t(), ref(), ref()) :: :ok
  def evaluate_code(evaluator, send_to, code, ref, prev_ref \\ :initial) when ref != :initial do
    GenServer.cast(evaluator, {:evaluate_code, send_to, code, ref, prev_ref})
  end

  @doc """
  Removes the evaluation identified by `ref` from history,
  so that further evaluations cannot use it.
  """
  @spec forget_evaluation(t(), ref()) :: :ok
  def forget_evaluation(evaluator, ref) do
    GenServer.cast(evaluator, {:forget_evaluation, ref})
  end

  ## Callbacks

  @impl true
  def init(opts) do
    formatter = Keyword.get(opts, :formatter, Evaluator.IdentityFormatter)

    {:ok, io_proxy} = Evaluator.IOProxy.start_link()

    # Use the dedicated IO device as the group leader,
    # so that it handles all :stdio operations.
    Process.group_leader(self(), io_proxy)
    {:ok, initial_state(formatter, io_proxy)}
  end

  defp initial_state(formatter, io_proxy) do
    %{
      formatter: formatter,
      io_proxy: io_proxy,
      contexts: %{initial: initial_context()}
    }
  end

  defp initial_context() do
    env = :elixir.env_for_eval([])
    %{binding: [], env: env}
  end

  @impl true
  def handle_cast({:evaluate_code, send_to, code, ref, prev_ref}, state) do
    Evaluator.IOProxy.configure(state.io_proxy, send_to, ref)

    context = Map.get(state.contexts, prev_ref, state.contexts.initial)

    case eval(code, context.binding, context.env) do
      {:ok, result, binding, env} ->
        result_context = %{binding: binding, env: env}
        new_contexts = Map.put(state.contexts, ref, result_context)
        new_state = %{state | contexts: new_contexts}

        send_evaluation_response(send_to, ref, {:ok, result}, state.formatter)
        {:noreply, new_state}

      {:error, kind, error, stacktrace} ->
        send_evaluation_response(send_to, ref, {:error, kind, error, stacktrace}, state.formatter)
        {:noreply, state}
    end
  end

  def handle_cast({:forget_evaluation, ref}, state) do
    new_state = %{state | contexts: Map.delete(state.contexts, ref)}
    {:noreply, new_state}
  end

  defp send_evaluation_response(send_to, ref, evaluation_response, formatter) do
    response = formatter.format(evaluation_response)
    send(send_to, {:evaluation_response, ref, response})
  end

  defp eval(code, binding, env) do
    try do
      quoted = Code.string_to_quoted!(code)
      {result, binding, env} = :elixir.eval_quoted(quoted, binding, env)

      {:ok, result, binding, env}
    catch
      kind, error ->
        {kind, error, stacktrace} = prepare_error(kind, error, __STACKTRACE__)
        {:error, kind, error, stacktrace}
    end
  end

  defp prepare_error(kind, error, stacktrace) do
    {error, stacktrace} = Exception.blame(kind, error, stacktrace)
    stacktrace = prune_stacktrace(stacktrace)
    {kind, error, stacktrace}
  end

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
end

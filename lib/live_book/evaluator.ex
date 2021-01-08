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

  use GenServer

  @type t :: GenServer.server()

  @type state :: %{
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

  # API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Synchronously parses and evaluates the given code.

  Any exceptions are captured, in which case this method returns an error.

  The evaluator stores the resulting binding and environment under `ref`.
  Any subsequent calls may specify `prev_ref` pointing to a previous evaluation,
  in which case the corresponding binding and environment are used during evaluation.
  """
  @spec evaluate_code(t(), String.t(), ref(), ref()) :: {:ok, any()} | {:error, any()}
  def evaluate_code(evaluator, code, ref, prev_ref \\ :initial) when ref != :initial do
    GenServer.call(evaluator, {:evaluate_code, code, ref, prev_ref}, :infinity)
  end

  # Callbacks

  @impl true
  def init(_opts) do
    {:ok, initial_state()}
  end

  defp initial_state() do
    %{contexts: %{initial: initial_context()}}
  end

  defp initial_context() do
    env = :elixir.env_for_eval([])
    %{binding: [], env: env}
  end

  @impl true
  def handle_call({:evaluate_code, code, ref, prev_ref}, _from, state) do
    context = state.contexts[prev_ref]

    case eval(code, context.binding, context.env) do
      {:ok, result, binding, env} ->
        result_context = %{binding: binding, env: env}

        new_contexts = Map.put(state.contexts, ref, result_context)
        new_state = %{state | contexts: new_contexts}

        {:reply, {:ok, result}, new_state}

      {:error, exception} ->
        {:reply, {:error, exception}, state}
    end
  end

  defp eval(code, binding, env) do
    try do
      {:ok, quoted} = Code.string_to_quoted(code)
      {result, binding, env} = :elixir.eval_quoted(quoted, binding, env)

      {:ok, result, binding, env}
    rescue
      exception ->
        {:error, exception}
    end
  end
end

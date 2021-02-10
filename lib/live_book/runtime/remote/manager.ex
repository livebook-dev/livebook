defmodule LiveBook.Runtime.Remote.Manager do
  @moduledoc false

  # The primary LiveBook process started on a remote node.
  #
  # This process is responsible for monitoring the owner
  # process on the main node and cleaning up if it terminates.
  # Also, this process keeps track of the evaluators
  # and spawns/terminates them whenever necessary for the evaluation.

  use GenServer

  alias LiveBook.Evaluator
  alias LiveBook.Runtime.Remote

  @name __MODULE__

  @await_owner_timeout 5_000

  def start(opts \\ []) do
    GenServer.start(__MODULE__, opts, name: @name)
  end

  @spec set_owner(node(), pid()) :: :ok
  def set_owner(node, owner) do
    GenServer.cast({@name, node}, {:set_owner, owner})
  end

  @spec evaluate_code(node(), String.t(), Evaluator.ref(), Evaluator.ref(), Evaluator.ref()) :: :ok
  def evaluate_code(node, code, container_ref, evaluation_ref, prev_evaluation_ref) do
    GenServer.cast(
      {@name, node},
      {:evaluate_code, code, container_ref, evaluation_ref, prev_evaluation_ref}
    )
  end

  @spec forget_evaluation(node(), Evaluator.ref(), Evaluator.ref()) :: :ok
  def forget_evaluation(node, container_ref, evaluation_ref) do
    GenServer.cast({@name, node}, {:forget_evaluation, container_ref, evaluation_ref})
  end

  @spec drop_container(node(), Evaluator.ref()) :: :ok
  def drop_container(node, container_ref) do
    GenServer.cast({@name, node}, {:drop_container, container_ref})
  end

  @spec stop(node()) :: :ok
  def stop(node) do
    GenServer.stop({@name, node})
  end

  @impl true
  def init(_opts) do
    Process.flag(:trap_exit, true)
    Remote.EvaluatorSupervisor.start_link()

    Process.send_after(self(), :check_owner, @await_owner_timeout)

    {:ok, %{owner: nil, evaluators: %{}}}
  end

  @impl true
  def terminate(_reason, _state) do
    Remote.unload_required_modules()

    :ok
  end

  @impl true
  def handle_info(:check_owner, state) do
    if state.owner do
      {:noreply, state}
    else
      {:stop, :no_owner, state}
    end
  end

  def handle_info({:DOWN, _, :process, owner, _}, %{owner: owner} = state) do
    {:stop, :normal, state}
  end

  def handle_info(_message, state), do: {:noreply, state}

  @impl true
  def handle_cast({:set_owner, owner}, state) do
    Process.monitor(owner)

    {:noreply, %{state | owner: owner}}
  end

  def handle_cast(
        {:evaluate_code, code, container_ref, evaluation_ref, prev_evaluation_ref},
        state
      ) do
    state = ensure_evaluator(state, container_ref)

    Evaluator.evaluate_code(
      state.evaluators[container_ref],
      state.owner,
      code,
      evaluation_ref,
      prev_evaluation_ref
    )

    {:noreply, state}
  end

  def handle_cast({:forget_evaluation, container_ref, evaluation_ref}, state) do
    with {:ok, evaluator} <- Map.fetch(state.evaluators, container_ref) do
      Evaluator.forget_evaluation(evaluator, evaluation_ref)
    end

    {:noreply, state}
  end

  def handle_cast({:drop_container, container_ref}, state) do
    state = discard_evaluator(state, container_ref)
    {:noreply, state}
  end

  defp ensure_evaluator(state, container_ref) do
    if Map.has_key?(state.evaluators, container_ref) do
      state
    else
      {:ok, evaluator} = Remote.EvaluatorSupervisor.start_evaluator()
      %{state | evaluators: Map.put(state.evaluators, container_ref, evaluator)}
    end
  end

  defp discard_evaluator(state, container_ref) do
    case Map.fetch(state.evaluators, container_ref) do
      {:ok, evaluator} ->
        Remote.EvaluatorSupervisor.terminate_evaluator(evaluator)
        %{state | evaluators: Map.delete(state.evaluators, container_ref)}

      :error ->
        state
    end
  end
end

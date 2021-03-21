defmodule LivebookTest.Runtime.SingleEvaluator do
  @moduledoc false

  # A simple runtime backed by a single evaluator process
  # running on the local node.
  #
  # This allows for working code evaluation without
  # starting a standalone Elixir runtime, so is neat for testing.

  defstruct [:evaluator]

  def init() do
    with {:ok, evaluator} <- Livebook.Evaluator.start_link() do
      {:ok, %__MODULE__{evaluator: evaluator}}
    end
  end
end

defimpl Livebook.Runtime, for: LivebookTest.Runtime.SingleEvaluator do
  alias Livebook.Evaluator

  def connect(runtime) do
    Process.monitor(runtime.evaluator)
  end

  def disconnect(_runtime), do: :ok

  def evaluate_code(
        runtime,
        code,
        _container_ref,
        evaluation_ref,
        prev_evaluation_ref \\ :initial,
        opts \\ []
      ) do
    Evaluator.evaluate_code(
      runtime.evaluator,
      self(),
      code,
      evaluation_ref,
      prev_evaluation_ref,
      opts
    )

    :ok
  end

  def forget_evaluation(runtime, _container_ref, evaluation_ref) do
    Evaluator.forget_evaluation(runtime.evaluator, evaluation_ref)
  end

  def drop_container(_runtime, _container_ref), do: :ok
end

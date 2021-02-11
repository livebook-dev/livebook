defmodule LiveBook.Runtime.ErlDist.EvaluatorSupervisor do
  @moduledoc false

  # Supervisor responsible for dynamically spawning
  # and terminating evaluator server processes.

  use DynamicSupervisor

  alias LiveBook.Evaluator

  @name __MODULE__

  def start_link(opts \\ []) do
    DynamicSupervisor.start_link(__MODULE__, opts, name: @name)
  end

  @impl true
  def init(_opts) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  @doc """
  Spawns a new evaluator.
  """
  @spec start_evaluator() :: {:ok, Evaluator.t()} | {:error, any()}
  def start_evaluator() do
    case DynamicSupervisor.start_child(@name, Evaluator) do
      {:ok, pid} -> {:ok, pid}
      {:ok, pid, _} -> {:ok, pid}
      :ignore -> {:error, :ignore}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Terminates the given evaluator.
  """
  @spec terminate_evaluator(Evaluator.t()) :: :ok
  def terminate_evaluator(evaluator) do
    DynamicSupervisor.terminate_child(@name, evaluator)
    :ok
  end
end

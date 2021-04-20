defmodule Livebook.Runtime.ErlDist.EvaluatorSupervisor do
  @moduledoc false

  # Supervisor responsible for dynamically spawning
  # and terminating evaluator server processes.

  use DynamicSupervisor

  alias Livebook.Evaluator

  def start_link(opts \\ []) do
    DynamicSupervisor.start_link(__MODULE__, opts)
  end

  @impl true
  def init(_opts) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  @doc """
  Spawns a new evaluator.
  """
  @spec start_evaluator(pid()) :: {:ok, Evaluator.t()} | {:error, any()}
  def start_evaluator(supervisor) do
    case DynamicSupervisor.start_child(
           supervisor,
           {Evaluator, [formatter: Evaluator.StringFormatter]}
         ) do
      {:ok, pid} -> {:ok, pid}
      {:ok, pid, _} -> {:ok, pid}
      :ignore -> {:error, :ignore}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Terminates the given evaluator.
  """
  @spec terminate_evaluator(pid(), Evaluator.t()) :: :ok
  def terminate_evaluator(supervisor, evaluator) do
    DynamicSupervisor.terminate_child(supervisor, evaluator)
    :ok
  end
end

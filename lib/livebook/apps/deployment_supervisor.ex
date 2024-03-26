defmodule Livebook.Apps.DeploymentSupervisor do
  # Supervision tree related to orchestrating app deployments in the
  # cluster.

  use Supervisor

  @name __MODULE__

  @doc """
  Starts the supervisor.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, {}, name: @name)
  end

  @doc """
  Starts a node-local instance of `Livebook.Apps.Manager`.
  """
  @spec start_manager() :: Supervisor.on_start_child()
  def start_manager() do
    Supervisor.start_child(@name, Livebook.Apps.Manager)
  end

  @impl true
  def init({}) do
    children = [
      # Start the supervisor dynamically managing apps
      {DynamicSupervisor, name: Livebook.AppSupervisor, strategy: :one_for_one},
      # Process group for app deployers
      %{id: Livebook.Apps.Deployer.PG, start: {:pg, :start_link, [Livebook.Apps.Deployer.PG]}},
      # Node-local app deployer
      Livebook.Apps.Deployer,
      # Node-local app manager watcher
      Livebook.Apps.ManagerWatcher
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end

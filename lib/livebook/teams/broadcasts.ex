defmodule Livebook.Teams.Broadcasts do
  alias Livebook.Teams

  @type broadcast :: :ok | {:error, term()}

  @agents_topic "teams:agents"
  @app_deployments_topic "teams:app_deployments"
  @clients_topic "teams:clients"
  @deployment_groups_topic "teams:deployment_groups"

  @doc """
  Subscribes to one or more subtopics in `"teams"`.

  ## Messages

  Topic `#{@agents_topic}`:

    * `{:agent_joined, Agent.t()}`
    * `{:agent_left, Agent.t()}`

  Topic `#{@app_deployments_topic}`:

    * `{:app_deployment_started, AppDeployment.t()}`
    * `{:app_deployment_stopped, AppDeployment.t()}`

  Topic `#{@clients_topic}`:

    * `{:client_connected, id}`

  Topic `#{@deployment_groups_topic}`:

    * `{:deployment_group_created, DeploymentGroup.t()}`
    * `{:deployment_group_update, DeploymentGroup.t()}`
    * `{:deployment_group_deleted, DeploymentGroup.t()}`

  """
  @spec subscribe(atom() | list(atom())) :: :ok | {:error, term()}
  def subscribe(topics) when is_list(topics) do
    for topic <- topics, do: subscribe(topic)

    :ok
  end

  def subscribe(topic) do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "teams:#{topic}")
  end

  @doc """
  Unsubscribes from `subscribe/0`.
  """
  @spec unsubscribe(atom() | list(atom())) :: :ok
  def unsubscribe(topics) when is_list(topics) do
    for topic <- topics, do: unsubscribe(topic)

    :ok
  end

  def unsubscribe(topic) do
    Phoenix.PubSub.unsubscribe(Livebook.PubSub, "teams:#{topic}")
  end

  @doc """
  Broadcasts under `#{@clients_topic}` topic when hub received a new client connection.
  """
  @spec client_connected(String.t()) :: broadcast()
  def client_connected(id) do
    broadcast(@clients_topic, {:client_connected, id})
  end

  @doc """
  Broadcasts under `#{@deployment_groups_topic}` topic when hub received a new deployment group.
  """
  @spec deployment_group_created(Teams.DeploymentGroup.t()) :: broadcast()
  def deployment_group_created(%Teams.DeploymentGroup{} = deployment_group) do
    broadcast(@deployment_groups_topic, {:deployment_group_created, deployment_group})
  end

  @doc """
  Broadcasts under `#{@deployment_groups_topic}` topic when hub received an updated deployment group.
  """
  @spec deployment_group_updated(Teams.DeploymentGroup.t()) :: broadcast()
  def deployment_group_updated(%Teams.DeploymentGroup{} = deployment_group) do
    broadcast(@deployment_groups_topic, {:deployment_group_updated, deployment_group})
  end

  @doc """
  Broadcasts under `#{@deployment_groups_topic}` topic when hub received a deleted deployment group.
  """
  @spec deployment_group_deleted(Teams.DeploymentGroup.t()) :: broadcast()
  def deployment_group_deleted(%Teams.DeploymentGroup{} = deployment_group) do
    broadcast(@deployment_groups_topic, {:deployment_group_deleted, deployment_group})
  end

  @doc """
  Broadcasts under `#{@app_deployments_topic}` topic when hub received to start a new app deployment.
  """
  @spec app_deployment_started(Teams.AppDeployment.t()) :: broadcast()
  def app_deployment_started(%Teams.AppDeployment{} = app_deployment) do
    broadcast(@app_deployments_topic, {:app_deployment_started, app_deployment})
  end

  @doc """
  Broadcasts under `#{@app_deployments_topic}` topic when hub received to stop an app deployment.
  """
  @spec app_deployment_stopped(Teams.AppDeployment.t()) :: broadcast()
  def app_deployment_stopped(%Teams.AppDeployment{} = app_deployment) do
    broadcast(@app_deployments_topic, {:app_deployment_stopped, app_deployment})
  end

  @doc """
  Broadcasts under `#{@agents_topic}` topic when hub received a new agent.
  """
  @spec agent_joined(Teams.Agent.t()) :: broadcast()
  def agent_joined(%Teams.Agent{} = agent) do
    broadcast(@agents_topic, {:agent_joined, agent})
  end

  @doc """
  Broadcasts under `#{@agents_topic}` topic when hub received a deleted agent.
  """
  @spec agent_left(Teams.Agent.t()) :: broadcast()
  def agent_left(%Teams.Agent{} = agent) do
    broadcast(@agents_topic, {:agent_left, agent})
  end

  defp broadcast(topic, message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, topic, message)
  end
end

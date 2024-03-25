defmodule Livebook.Teams.Broadcasts do
  alias Livebook.Teams.{AppDeployment, DeploymentGroup}

  @type broadcast :: :ok | {:error, term()}

  @deployment_groups_topic "teams:deployment_groups"
  @app_deployments_topic "teams:app_deployments"

  @doc """
  Subscribes to one or more subtopics in `"teams"`.

  ## Messages

  Topic `#{@deployment_groups_topic}`:

    * `{:deployment_group_created, DeploymentGroup.t()}`
    * `{:deployment_group_updated, DeploymentGroup.t()}`
    * `{:deployment_group_deleted, DeploymentGroup.t()}`

  Topic `#{@app_deployments_topic}`:

    * `{:app_deployment_created, AppDeployment.t()}`

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
  Broadcasts under `#{@deployment_groups_topic}` topic when hub received a new deployment group.
  """
  @spec deployment_group_created(DeploymentGroup.t()) :: broadcast()
  def deployment_group_created(%DeploymentGroup{} = deployment_group) do
    broadcast(@deployment_groups_topic, {:deployment_group_created, deployment_group})
  end

  @doc """
  Broadcasts under `#{@deployment_groups_topic}` topic when hub received an updated deployment group.
  """
  @spec deployment_group_updated(DeploymentGroup.t()) :: broadcast()
  def deployment_group_updated(%DeploymentGroup{} = deployment_group) do
    broadcast(@deployment_groups_topic, {:deployment_group_updated, deployment_group})
  end

  @doc """
  Broadcasts under `#{@deployment_groups_topic}` topic when hub received a deleted deployment group.
  """
  @spec deployment_group_deleted(DeploymentGroup.t()) :: broadcast()
  def deployment_group_deleted(%DeploymentGroup{} = deployment_group) do
    broadcast(@deployment_groups_topic, {:deployment_group_deleted, deployment_group})
  end

  @doc """
  Broadcasts under `#{@app_deployments_topic}` topic when hub received a new app deployment.
  """
  @spec app_deployment_created(AppDeployment.t()) :: broadcast()
  def app_deployment_created(%AppDeployment{} = app_deployment) do
    broadcast(@app_deployments_topic, {:app_deployment_created, app_deployment})
  end

  defp broadcast(topic, message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, topic, message)
  end
end

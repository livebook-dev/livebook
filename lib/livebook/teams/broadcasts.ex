defmodule Livebook.Teams.Broadcasts do
  alias Livebook.Teams.DeploymentGroup

  @type broadcast :: :ok | {:error, term()}

  @deployment_groups_topic "teams:deployment_groups"

  @doc """
  Subscribes to one or more subtopics in `"teams"`.
  ## Messages
  Topic `teams:deployment_groups`:
    * `{:deployment_group_created, DeploymentGroup.t()}`
    * `{:deployment_group_updated, DeploymentGroup.t()}`
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

  defp broadcast(topic, message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, topic, message)
  end
end

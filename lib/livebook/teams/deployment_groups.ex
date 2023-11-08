defmodule Livebook.Teams.DeploymentGroups do
  # Shared deployment group functionality across all hubs.

  alias Livebook.Teams.DeploymentGroups.DeploymentGroup
  alias Livebook.Hubs.TeamClient
  alias Livebook.Teams

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking deployment group changes.
  """
  @spec change_deployment_group(DeploymentGroup.t(), map()) :: Ecto.Changeset.t()
  def change_deployment_group(%DeploymentGroup{} = deployment_group, attrs \\ %{}) do
    DeploymentGroup.changeset(deployment_group, attrs)
  end

  @doc """
  Updates deployment group with the given changes.
  """
  @spec update_deployment_group(DeploymentGroup.t(), map()) ::
          {:ok, DeploymentGroup.t()} | {:error, Ecto.Changeset.t()}
  def update_deployment_group(%DeploymentGroup{} = deployment_group, attrs) do
    deployment_group
    |> DeploymentGroup.changeset(attrs)
    |> Ecto.Changeset.apply_action(:update)
  end

  def update_deployment_group(team, deployment_group),
    do: Teams.update_deployment_group(team, deployment_group)

  def create_deployment_group(team, deployment_group),
    do: Teams.create_deployment_group(team, deployment_group)

  def delete_deployment_group(team, deployment_group),
    do: Teams.delete_deployment_group(team, deployment_group)

  def get_deployment_groups(team) do
    TeamClient.get_deployment_groups(team.id)
  end
end

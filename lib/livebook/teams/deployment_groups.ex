defmodule Livebook.Teams.DeploymentGroups do
  # Shared deployment group functionality across all hubs.

  alias Livebook.Teams.DeploymentGroups.DeploymentGroup
  alias Livebook.Hubs.TeamClient
  alias Livebook.Hubs.Team
  alias Livebook.Teams.Requests

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

  @spec update_deployment_group(Team.t(), DeploymentGroup.t()) ::
          {:ok, pos_integer()}
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def update_deployment_group(%Team{} = team, deployment_group) do
    case Requests.update_deployment_group(team, deployment_group) do
      {:ok, %{"id" => id}} ->
        {:ok, id}

      {:error, %{"errors" => errors}} ->
        {:error, Requests.add_errors(deployment_group, errors)}

      any ->
        any
    end
  end

  @doc """
  Creates a Deployment Group.
  """
  @spec create_deployment_group(Team.t(), DeploymentGroup.t()) ::
          {:ok, pos_integer()}
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def create_deployment_group(%Team{} = team, deployment_group) do
    case Requests.create_deployment_group(team, deployment_group) do
      {:ok, %{"id" => id}} ->
        {:ok, id}

      {:error, %{"errors" => errors}} ->
        {:error, Requests.add_errors(deployment_group, errors)}

      any ->
        any
    end
  end

  @doc """
  Deletes a Deployment Group.
  """
  @spec delete_deployment_group(Team.t(), DeploymentGroup.t()) ::
          :ok
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def delete_deployment_group(%Team{} = team, deployment_group) do
    case Requests.delete_deployment_group(team, deployment_group) do
      {:ok, _} ->
        :ok

      {:error, %{"errors" => errors}} ->
        {:error, Requests.add_errors(deployment_group, errors)}

      any ->
        any
    end
  end

  def get_deployment_groups(team) do
    TeamClient.get_deployment_groups(team.id)
  end
end

defmodule Livebook.Teams do
  # This is the Livebook Teams interface which is not part of Hubs.

  alias Livebook.Hubs
  alias Livebook.Hubs.Team
  alias Livebook.Hubs.TeamClient
  alias Livebook.Teams
  alias Livebook.Teams.Org
  alias Livebook.Teams.Requests

  import Ecto.Changeset,
    only: [add_error: 3, apply_action: 2, apply_action!: 2, get_field: 2]

  @prefix Org.teams_key_prefix()

  @doc """
  Creates an Org.

  With success, returns the response from Livebook Teams API to continue the org creation flow.
  Otherwise, it will return an error tuple with changeset.
  """
  @spec create_org(Org.t(), map()) ::
          {:ok, map()}
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def create_org(%Org{} = org, attrs) do
    create_org_request(org, attrs, &Requests.create_org/1)
  end

  @doc """
  Joins an Org.

  With success, returns the response from Livebook Teams API to continue the org joining flow.
  Otherwise, it will return an error tuple with changeset.
  """
  @spec join_org(Org.t(), map()) ::
          {:ok, map()}
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def join_org(%Org{} = org, attrs) do
    create_org_request(org, attrs, &Requests.join_org/1)
  end

  defp create_org_request(%Org{} = org, attrs, callback) when is_function(callback, 1) do
    changeset = Org.changeset(org, attrs)

    with {:ok, %Org{} = org} <- apply_action(changeset, :insert) do
      case callback.(org) do
        {:ok, response} ->
          {:ok, response}

        {:error, %{"errors" => errors}} ->
          errors = map_teams_field_to_livebook_field(errors, "key_hash", "teams_key")
          {:error, changeset |> add_external_errors(errors) |> Map.replace!(:action, :insert)}

        any ->
          any
      end
    end
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking org changes.
  """
  @spec change_org(Org.t(), map()) :: Ecto.Changeset.t()
  def change_org(%Org{} = org, attrs \\ %{}) do
    Org.changeset(org, attrs)
  end

  @doc """
  Send a request to Livebook Teams API to get an org request.
  """
  @spec get_org_request_completion_data(Org.t(), binary()) ::
          {:ok, map() | :awaiting_confirmation}
          | {:error, :expired}
          | {:transport_error, String.t()}
  def get_org_request_completion_data(%Org{id: id}, device_code) do
    case Requests.get_org_request_completion_data(id, device_code) do
      {:ok, %{"status" => "awaiting_confirmation"}} -> {:ok, :awaiting_confirmation}
      {:ok, completion_data} -> {:ok, completion_data}
      {:error, %{"status" => "expired"}} -> {:error, :expired}
      any -> any
    end
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking hub changes.
  """
  @spec change_hub(Team.t(), map()) :: Ecto.Changeset.t()
  def change_hub(%Team{} = team, attrs \\ %{}) do
    Team.update_changeset(team, attrs)
  end

  @doc """
  Creates a Hub.

  It notifies interested processes about hub metadata data change.
  """
  @spec create_hub!(map()) :: Team.t()
  def create_hub!(attrs) do
    changeset = Team.creation_changeset(Team.new(), attrs)
    team = apply_action!(changeset, :insert)

    Hubs.save_hub(team)
  end

  @doc """
  Updates a Hub.

  With success, notifies interested processes about hub metadata data change.
  Otherwise, it will return an error tuple with changeset.
  """
  @spec update_hub(Team.t(), map()) :: {:ok, Team.t()} | {:error, Ecto.Changeset.t()}
  def update_hub(%Team{} = team, attrs) do
    changeset = Team.update_changeset(team, attrs)
    id = get_field(changeset, :id)

    if Hubs.hub_exists?(id) do
      with {:ok, struct} <- apply_action(changeset, :update) do
        {:ok, Hubs.save_hub(struct)}
      end
    else
      {:error, add_error(changeset, :hub_name, "does not exists")}
    end
  end

  @doc """
  Encrypts the given value with Teams key derived keys.
  """
  @spec encrypt(String.t() | nil, bitstring()) :: String.t()
  def encrypt(value, _secret) when value in ["", nil], do: value

  def encrypt(value, secret) do
    Plug.Crypto.MessageEncryptor.encrypt(value, secret, "unused")
  end

  @doc """
  Decrypts the given encrypted value with Teams key derived keys.
  """
  @spec decrypt(String.t() | nil, bitstring()) :: {:ok, String.t()} | :error
  def decrypt(value, _secret) when value in ["", nil], do: value

  def decrypt(encrypted_value, secret) do
    Plug.Crypto.MessageEncryptor.decrypt(encrypted_value, secret, "unused")
  end

  @doc """
  Derives the secret and sign secret from given `teams_key`.
  """
  @spec derive_key(String.t()) :: bitstring()
  def derive_key(@prefix <> teams_key) do
    binary_key = Base.url_decode64!(teams_key, padding: false)
    Plug.Crypto.KeyGenerator.generate(binary_key, "notebook secret", cache: Plug.Crypto.Keys)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking deployment group changes.
  """
  @spec change_deployment_group(Teams.DeploymentGroup.t(), map()) :: Ecto.Changeset.t()
  def change_deployment_group(%Teams.DeploymentGroup{} = deployment_group, attrs \\ %{}) do
    Teams.DeploymentGroup.changeset(deployment_group, attrs)
  end

  @doc """
  Creates a Deployment Group.
  """
  @spec create_deployment_group(Team.t(), map()) ::
          {:ok, Teams.DeploymentGroup.t()}
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def create_deployment_group(%Team{} = team, attrs) do
    changeset = Teams.DeploymentGroup.changeset(%Teams.DeploymentGroup{}, attrs)

    with {:ok, %Teams.DeploymentGroup{} = deployment_group} <- apply_action(changeset, :insert) do
      case Requests.create_deployment_group(team, deployment_group) do
        {:ok, %{"id" => id}} ->
          {:ok, %{deployment_group | id: to_string(id)}}

        {:error, %{"errors" => errors}} ->
          {:error,
           changeset
           |> add_external_errors(errors)
           |> Map.replace!(:action, :insert)}

        any ->
          any
      end
    end
  end

  @doc """
  Gets a list of deployment groups for a given Hub.
  """
  @spec get_deployment_groups(Team.t()) :: list(Teams.DeploymentGroup.t())
  def get_deployment_groups(team) do
    TeamClient.get_deployment_groups(team.id)
  end

  @doc """
  Creates a new app deployment.
  """
  @spec deploy_app(Team.t(), Teams.AppDeployment.t()) ::
          :ok
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def deploy_app(%Team{} = team, %Teams.AppDeployment{} = app_deployment) do
    case Requests.deploy_app(team, app_deployment) do
      {:ok, %{"id" => _id}} ->
        :ok

      {:error, %{"errors" => %{"detail" => error}}} ->
        {:error, add_external_errors(app_deployment, %{"file" => [error]})}

      {:error, %{"errors" => errors}} ->
        {:error, add_external_errors(app_deployment, errors)}

      any ->
        any
    end
  end

  @doc """
  Gets a list of app deployments for a given Hub.
  """
  @spec get_app_deployments(Team.t()) :: list(Teams.AppDeployment.t())
  def get_app_deployments(team) do
    TeamClient.get_app_deployments(team.id)
  end

  @doc """
  Gets a list of agents for a given Hub.
  """
  @spec get_agents(Team.t()) :: list(Teams.Agent.t())
  def get_agents(team) do
    TeamClient.get_agents(team.id)
  end

  @doc """
  Gets a list of environment variables for a given Hub.
  """
  @spec get_environment_variables(Team.t()) :: list(Teams.Agent.t())
  def get_environment_variables(team) do
    TeamClient.get_environment_variables(team.id)
  end

  defp map_teams_field_to_livebook_field(map, teams_field, livebook_field) do
    if value = map[teams_field] do
      Map.put_new(map, livebook_field, value)
    else
      map
    end
  end

  defp add_external_errors(%Ecto.Changeset{data: %struct{}} = changeset, errors_map) do
    errors = Requests.to_error_list(struct, errors_map)
    Livebook.Utils.put_changeset_errors(changeset, errors)
  end

  defp add_external_errors(struct, errors_map) do
    struct |> Ecto.Changeset.change() |> add_external_errors(errors_map)
  end
end

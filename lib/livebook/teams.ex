defmodule Livebook.Teams do
  # This is the Livebook Teams interface which is not part of Hubs.

  alias Livebook.Hubs
  alias Livebook.Hubs.Team
  alias Livebook.Hubs.TeamClient
  alias Livebook.Teams.{AgentKey, AppDeployment, DeploymentGroup, Org, Requests}

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

    with {:ok, %Org{} = org} <- apply_action(changeset, :insert),
         {:ok, response} <- callback.(org) do
      {:ok, response}
    else
      {:error, %Ecto.Changeset{} = changeset} ->
        {:error, changeset}

      {:error, %{"errors" => errors_map}} ->
        errors_map =
          if errors = errors_map["key_hash"],
            do: Map.put_new(errors_map, "teams_key", errors),
            else: errors_map

        {:error, add_org_errors(changeset, errors_map)}

      any ->
        any
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

  defp add_org_errors(%Ecto.Changeset{} = changeset, errors_map) do
    Requests.add_errors(changeset, Org.__schema__(:fields), errors_map)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking deployment group changes.
  """
  @spec change_deployment_group(DeploymentGroup.t(), map()) :: Ecto.Changeset.t()
  def change_deployment_group(%DeploymentGroup{} = deployment_group, attrs \\ %{}) do
    DeploymentGroup.changeset(deployment_group, attrs)
  end

  @doc """
  Updates a deployment group with the given changes.
  """
  @spec update_deployment_group(Team.t(), DeploymentGroup.t()) ::
          {:ok, pos_integer()}
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def update_deployment_group(%Team{} = team, deployment_group) do
    case Requests.update_deployment_group(team, deployment_group) do
      {:ok, %{"id" => id}} -> {:ok, id}
      {:error, %{"errors" => errors}} -> {:error, Requests.add_errors(deployment_group, errors)}
      any -> any
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
      {:ok, %{"id" => id}} -> {:ok, id}
      {:error, %{"errors" => errors}} -> {:error, Requests.add_errors(deployment_group, errors)}
      any -> any
    end
  end

  @doc """
  Creates an Agent Key.
  """
  @spec create_agent_key(Team.t(), DeploymentGroup.t()) ::
          {:ok, pos_integer()}
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def create_agent_key(%Team{} = team, deployment_group) do
    case Requests.create_agent_key(team, deployment_group) do
      {:ok, _} -> :ok
      {:error, %{"errors" => errors}} -> {:error, Requests.add_errors(deployment_group, errors)}
      any -> any
    end
  end

  @doc """
  Deletes an Agent Key.
  """
  @spec delete_agent_key(Team.t(), AgentKey.t()) ::
          :ok
          | {:error, Ecto.Changeset.t()}
          | {:transport_error, String.t()}
  def delete_agent_key(%Team{} = team, agent_key) do
    case Requests.delete_agent_key(team, agent_key) do
      {:ok, _} -> :ok
      {:error, %{"errors" => errors}} -> {:error, Requests.add_errors(agent_key, errors)}
      any -> any
    end
  end

  @doc """
  Gets a list of deployment groups for a given Hub.
  """
  @spec get_deployment_groups(Team.t()) :: list(DeploymentGroup.t())
  def get_deployment_groups(team) do
    TeamClient.get_deployment_groups(team.id)
  end

  @doc """
  Gets a list of agent keys for a given Hub and deployment group id.
  """
  @spec get_agent_keys(Team.t(), String.t()) :: list(AgentKey.t())
  def get_agent_keys(team, deployment_group_id) do
    deployment_groups = TeamClient.get_deployment_groups(team.id)

    if deployment_group = Enum.find(deployment_groups, &(&1.id == deployment_group_id)) do
      deployment_group.agent_keys
    else
      []
    end
  end

  @doc """
  Creates a new app deployment.
  """
  @spec deploy_app(Team.t(), String.t(), String.t(), String.t(), AppDeployment.zip_files()) ::
          :ok | {:error, String.t()} | {:transport_error, String.t()}
  def deploy_app(%Team{} = team, title, slug, deployment_group_id, files) do
    with {:ok, app_deployment} <- AppDeployment.new(title, slug, deployment_group_id, files) do
      case Requests.deploy_app(team, app_deployment) do
        {:ok, %{"id" => _id}} -> :ok
        {:error, %{"errors" => _}} -> {:error, Requests.error_message()}
        any -> any
      end
    end
  end
end

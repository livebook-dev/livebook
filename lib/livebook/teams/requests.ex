defmodule Livebook.Teams.Requests do
  alias Livebook.FileSystem
  alias Livebook.FileSystems
  alias Livebook.Hubs.Team
  alias Livebook.Secrets.Secret
  alias Livebook.Teams
  alias Livebook.Teams.Org
  alias Livebook.Utils.HTTP
  alias Livebook.Teams.DeploymentGroup

  @doc """
  Send a request to Livebook Team API to create a new org.
  """
  @spec create_org(Org.t()) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def create_org(org) do
    post("/api/v1/org-request", %{name: org.name, key_hash: Org.key_hash(org)})
  end

  @doc """
  Send a request to Livebook Team API to join an org.
  """
  @spec join_org(Org.t()) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def join_org(org) do
    post("/api/v1/org-request/join", %{name: org.name, key_hash: Org.key_hash(org)})
  end

  @doc """
  Send a request to Livebook Team API to get an org request.
  """
  @spec get_org_request_completion_data(pos_integer(), binary) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def get_org_request_completion_data(id, device_code) do
    get("/api/v1/org-request/#{id}?device_code=#{device_code}")
  end

  @doc """
  Send a request to Livebook Team API to sign the given payload.
  """
  @spec org_sign(Team.t(), String.t()) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def org_sign(team, payload) do
    post("/api/v1/org/sign", %{payload: payload}, team)
  end

  @doc """
  Send a request to Livebook Team API to create a secret.
  """
  @spec create_secret(Team.t(), Secret.t()) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def create_secret(team, %{deployment_group_id: nil} = secret) do
    secret_key = Teams.derive_key(team.teams_key)
    secret_value = Teams.encrypt(secret.value, secret_key)

    post("/api/v1/org/secrets", %{name: secret.name, value: secret_value}, team)
  end

  def create_secret(team, secret) do
    secret_key = Teams.derive_key(team.teams_key)
    secret_value = Teams.encrypt(secret.value, secret_key)

    params = %{
      name: secret.name,
      value: secret_value,
      deployment_group_id: secret.deployment_group_id
    }

    post("/api/v1/org/deployment-groups/secrets", params, team)
  end

  @doc """
  Send a request to Livebook Team API to update a secret.
  """
  @spec update_secret(Team.t(), Secret.t()) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def update_secret(team, %{deployment_group_id: nil} = secret) do
    secret_key = Teams.derive_key(team.teams_key)
    secret_value = Teams.encrypt(secret.value, secret_key)

    put("/api/v1/org/secrets", %{name: secret.name, value: secret_value}, team)
  end

  def update_secret(team, secret) do
    secret_key = Teams.derive_key(team.teams_key)
    secret_value = Teams.encrypt(secret.value, secret_key)

    params = %{
      name: secret.name,
      value: secret_value,
      deployment_group_id: secret.deployment_group_id
    }

    put("/api/v1/org/deployment-groups/secrets", params, team)
  end

  @doc """
  Send a request to Livebook Team API to delete a secret.
  """
  @spec delete_secret(Team.t(), Secret.t()) ::
          {:ok, String.t()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def delete_secret(team, %{deployment_group_id: nil} = secret) do
    delete("/api/v1/org/secrets", %{name: secret.name}, team)
  end

  def delete_secret(team, secret) do
    params = %{name: secret.name, deployment_group_id: secret.deployment_group_id}

    delete("/api/v1/org/deployment-groups/secrets", params, team)
  end

  @doc """
  Send a request to Livebook Team API to create a file system.
  """
  @spec create_file_system(Team.t(), FileSystem.t()) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def create_file_system(team, file_system) do
    secret_key = Teams.derive_key(team.teams_key)

    type = FileSystems.type(file_system)
    %{name: name} = FileSystem.external_metadata(file_system)
    attrs = FileSystem.dump(file_system)
    json = Jason.encode!(attrs)

    params = %{
      name: name,
      type: to_string(type),
      value: Teams.encrypt(json, secret_key)
    }

    post("/api/v1/org/file-systems", params, team)
  end

  @doc """
  Send a request to Livebook Team API to update a file system.
  """
  @spec update_file_system(Team.t(), FileSystem.t()) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def update_file_system(team, file_system) do
    secret_key = Teams.derive_key(team.teams_key)

    type = FileSystems.type(file_system)
    %{name: name} = FileSystem.external_metadata(file_system)
    attrs = FileSystem.dump(file_system)
    json = Jason.encode!(attrs)

    params = %{
      id: file_system.external_id,
      name: name,
      type: to_string(type),
      value: Teams.encrypt(json, secret_key)
    }

    put("/api/v1/org/file-systems", params, team)
  end

  @doc """
  Send a request to Livebook Team API to delete a file system.
  """
  @spec delete_file_system(Team.t(), FileSystem.t()) ::
          {:ok, String.t()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def delete_file_system(team, file_system) do
    delete("/api/v1/org/file-systems", %{id: file_system.external_id}, team)
  end

  @doc """
  Send a request to Livebook Team API to create a deployment group.
  """
  @spec create_deployment_group(Team.t(), DeploymentGroup.t()) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def create_deployment_group(team, deployment_group) do
    params = %{
      name: deployment_group.name,
      mode: deployment_group.mode,
      clustering: deployment_group.clustering,
      zta_provider: deployment_group.zta_provider,
      zta_key: deployment_group.zta_key
    }

    post("/api/v1/org/deployment-groups", params, team)
  end

  @doc """
  Send a request to Livebook Team API to update a deployment group.
  """
  @spec update_deployment_group(Team.t(), DeploymentGroup.t()) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def update_deployment_group(team, deployment_group) do
    params = %{
      id: deployment_group.id,
      name: deployment_group.name,
      mode: deployment_group.mode,
      clustering: deployment_group.clustering,
      zta_provider: deployment_group.zta_provider,
      zta_key: deployment_group.zta_key
    }

    put("/api/v1/org/deployment-groups", params, team)
  end

  @doc """
  Send a request to Livebook Team API to delete a deployment group.
  """
  @spec delete_deployment_group(Team.t(), DeploymentGroup.t()) ::
          {:ok, String.t()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def delete_deployment_group(team, deployment_group) do
    delete("/api/v1/org/deployment-groups", %{id: deployment_group.id}, team)
  end

  @doc """
  Add requests errors to a `changeset` for the given `fields`.
  """
  def add_errors(%Ecto.Changeset{} = changeset, fields, errors_map) do
    for {key, errors} <- errors_map,
        field = String.to_atom(key),
        field in fields,
        error <- errors,
        reduce: changeset,
        do: (acc -> Ecto.Changeset.add_error(acc, field, error))
  end

  @doc """
  Add requests errors to a struct.
  """
  def add_errors(%struct{} = value, errors_map) do
    value |> Ecto.Changeset.change() |> add_errors(struct.__schema__(:fields), errors_map)
  end

  defp auth_headers(team) do
    token =
      if team.user_id do
        "#{team.user_id}:#{team.org_id}:#{team.org_key_id}:#{team.session_token}"
      else
        "#{team.session_token}:#{Livebook.Config.agent_name()}:#{team.org_id}:#{team.org_key_id}"
      end

    [
      {"x-lb-version", Livebook.Config.app_version()},
      {"authorization", "Bearer " <> token}
    ]
  end

  defp post(path, json, team \\ nil) do
    body = {"application/json", Jason.encode!(json)}
    headers = if team, do: auth_headers(team), else: []

    request(:post, path, body: body, headers: headers)
    |> dispatch_messages(team)
  end

  defp put(path, json, team) do
    body = {"application/json", Jason.encode!(json)}

    request(:put, path, body: body, headers: auth_headers(team))
    |> dispatch_messages(team)
  end

  defp delete(path, json, team) do
    body = {"application/json", Jason.encode!(json)}

    request(:delete, path, body: body, headers: auth_headers(team))
    |> dispatch_messages(team)
  end

  defp get(path, params \\ %{}) do
    query_string = URI.encode_query(params)
    path = if query_string != "", do: "#{path}?#{query_string}", else: path

    request(:get, path, headers: [])
  end

  defp request(method, path, opts) do
    endpoint = Livebook.Config.teams_url()
    url = endpoint <> path

    case HTTP.request(method, url, opts) do
      {:ok, 204, _headers, body} ->
        {:ok, body}

      {:ok, status, headers, body} when status in 200..299 ->
        if json?(headers),
          do: {:ok, Jason.decode!(body)},
          else: {:error, body}

      {:ok, status, headers, body} when status in [410, 422] ->
        if json?(headers),
          do: {:error, Jason.decode!(body)},
          else: {:transport_error, body}

      {:ok, 401, _headers, _body} ->
        {:transport_error,
         "You are not authorized to perform this action, make sure you have the access or you are not in a Livebook Agent instance"}

      _otherwise ->
        {:transport_error,
         "Something went wrong, try again later or please file a bug if it persists"}
    end
  end

  defp dispatch_messages({:ok, %{"messages" => _} = body}, %Livebook.Hubs.Team{} = team) do
    {messages, body} = Map.pop!(body, "messages")

    for message <- messages do
      %{type: event} =
        message
        |> Base.url_decode64!(padding: false)
        |> LivebookProto.Event.decode()

      Livebook.Hubs.TeamClient.handle_event(team.id, event)
    end

    {:ok, body}
  end

  defp dispatch_messages(result, _), do: result

  defp json?(headers) do
    HTTP.fetch_content_type(headers) == {:ok, "application/json"}
  end
end

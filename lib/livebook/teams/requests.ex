defmodule Livebook.Teams.Requests do
  alias Livebook.FileSystem
  alias Livebook.FileSystems
  alias Livebook.Hubs.Team
  alias Livebook.Secrets.Secret
  alias Livebook.Teams

  @deploy_key_prefix Teams.Constants.deploy_key_prefix()
  @error_message "Something went wrong, try again later or please file a bug if it persists"
  @unauthorized_error_message "You are not authorized to perform this action, make sure you have the access and you are not in a Livebook App Server/Offline instance"

  @typep api_result :: {:ok, map()} | error_result()
  @typep error_result :: {:error, map() | String.t()} | {:transport_error, String.t()}

  @doc false
  def error_message(), do: @error_message

  @doc """
  Send a request to Livebook Team API to create a new org.
  """
  @spec create_org(Teams.Org.t()) :: api_result()
  def create_org(org) do
    post("/api/v1/org-request", %{name: org.name, key_hash: Teams.Org.key_hash(org)})
  end

  @doc """
  Send a request to Livebook Team API to join an org.
  """
  @spec join_org(Teams.Org.t()) :: api_result()
  def join_org(org) do
    post("/api/v1/org-request/join", %{name: org.name, key_hash: Teams.Org.key_hash(org)})
  end

  @doc """
  Send a request to Livebook Team API to get an org request.
  """
  @spec get_org_request_completion_data(pos_integer(), binary) :: api_result()
  def get_org_request_completion_data(id, device_code) do
    get("/api/v1/org-request/#{id}?device_code=#{device_code}")
  end

  @doc """
  Send a request to Livebook Team API to sign the given payload.
  """
  @spec org_sign(Team.t(), String.t()) :: api_result()
  def org_sign(team, payload) do
    post("/api/v1/org/sign", %{payload: payload}, team)
  end

  @doc """
  Send a request to Livebook Team API to create a secret.
  """
  @spec create_secret(Team.t(), Secret.t()) :: api_result()
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
  @spec update_secret(Team.t(), Secret.t()) :: api_result()
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
  @spec delete_secret(Team.t(), Secret.t()) :: api_result()
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
  @spec create_file_system(Team.t(), FileSystem.t()) :: api_result()
  def create_file_system(team, file_system) do
    secret_key = Teams.derive_key(team.teams_key)

    type = FileSystems.type(file_system)
    %{name: name} = FileSystem.external_metadata(file_system)
    attrs = FileSystem.dump(file_system)
    json = JSON.encode!(attrs)

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
  @spec update_file_system(Team.t(), FileSystem.t()) :: api_result()
  def update_file_system(team, file_system) do
    secret_key = Teams.derive_key(team.teams_key)

    type = FileSystems.type(file_system)
    %{name: name} = FileSystem.external_metadata(file_system)
    attrs = FileSystem.dump(file_system)
    json = JSON.encode!(attrs)

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
  @spec delete_file_system(Team.t(), FileSystem.t()) :: api_result()
  def delete_file_system(team, file_system) do
    delete("/api/v1/org/file-systems", %{id: file_system.external_id}, team)
  end

  @doc """
  Send a request to Livebook Team API to create a deployment group.
  """
  @spec create_deployment_group(Team.t(), Teams.DeploymentGroup.t()) :: api_result()
  def create_deployment_group(team, deployment_group) do
    params = %{
      name: deployment_group.name,
      mode: deployment_group.mode,
      clustering: deployment_group.clustering,
      url: deployment_group.url
    }

    post("/api/v1/org/deployment-groups", params, team)
  end

  @doc """
  Send a request to Livebook Team API to deploy an app.
  """
  @spec deploy_app(Team.t(), Teams.AppDeployment.t()) :: api_result()
  def deploy_app(team, app_deployment) do
    secret_key = Teams.derive_key(team.teams_key)

    params = %{
      title: app_deployment.title,
      slug: app_deployment.slug,
      multi_session: app_deployment.multi_session,
      access_type: app_deployment.access_type,
      deployment_group_id: app_deployment.deployment_group_id,
      sha: app_deployment.sha
    }

    encrypted_content = Teams.encrypt(app_deployment.file, secret_key)
    upload("/api/v1/org/apps", encrypted_content, params, team)
  end

  @doc """
  Send a request to Livebook Team API to download an app revision.
  """
  @spec download_revision(Team.t(), Teams.AppDeployment.t()) :: {:ok, binary()} | error_result()
  def download_revision(team, app_deployment) do
    params = %{id: app_deployment.id, deployment_group_id: app_deployment.deployment_group_id}
    get("/api/v1/org/apps", params, team)
  end

  @doc """
  Send a request to Livebook Team API to create a new auth request.
  """
  @spec create_auth_request(Team.t()) :: api_result()
  def create_auth_request(team) do
    post("/api/v1/org/identity", %{}, team)
  end

  @doc """
  Send a request to Livebook Team API to get the access token from given auth request code.
  """
  @spec retrieve_access_token(Team.t(), String.t()) :: api_result()
  def retrieve_access_token(team, code) do
    post("/api/v1/org/identity/token", %{code: code}, team)
  end

  @doc """
  Send a request to Livebook Team API to get the user information from given access token.
  """
  @spec get_user_info(Team.t(), String.t()) :: api_result()
  def get_user_info(team, access_token) do
    get("/api/v1/org/identity", %{access_token: access_token}, team)
  end

  @doc """
  Send a request to Livebook Team API to return a session using a deploy key.
  """
  @spec fetch_cli_session(map()) :: api_result()
  def fetch_cli_session(config) do
    post("/api/v1/cli/auth", %{}, config)
  end

  @doc """
  Send a request to Livebook Team API to deploy an app using a deploy key.
  """
  @spec deploy_app_from_cli(Team.t(), Teams.AppDeployment.t(), String.t()) :: api_result()
  def deploy_app_from_cli(team, app_deployment, deployment_group_name) do
    secret_key = Teams.derive_key(team.teams_key)

    params = %{
      title: app_deployment.title,
      slug: app_deployment.slug,
      multi_session: app_deployment.multi_session,
      access_type: app_deployment.access_type,
      deployment_group_name: deployment_group_name,
      sha: app_deployment.sha
    }

    encrypted_content = Teams.encrypt(app_deployment.file, secret_key)
    upload("/api/v1/cli/org/apps", encrypted_content, params, team)
  end

  @doc """
  Normalizes errors map into errors for the given schema.
  """
  @spec to_error_list(module(), %{String.t() => list(String.t())}) ::
          list({atom(), list(String.t())})
  def to_error_list(struct, errors_map) do
    fields = struct.__schema__(:fields) |> MapSet.new()

    for {key, errors} <- errors_map,
        field = String.to_atom(key),
        field in fields,
        do: {field, errors}
  end

  defp post(path, json, team \\ nil) do
    build_req(team)
    |> Req.post(url: path, json: json)
    |> handle_response()
    |> dispatch_messages(team)
  end

  defp put(path, json, team) do
    build_req(team)
    |> Req.put(url: path, json: json)
    |> handle_response()
    |> dispatch_messages(team)
  end

  defp delete(path, json, team) do
    build_req(team)
    |> Req.delete(url: path, json: json)
    |> handle_response()
    |> dispatch_messages(team)
  end

  defp get(path, params \\ %{}, team \\ nil) do
    build_req(team)
    |> Req.get(url: path, params: params)
    |> handle_response()
  end

  defp upload(path, content, params, team) do
    build_req(team)
    |> Req.Request.put_header("content-length", "#{byte_size(content)}")
    |> Req.post(url: path, params: params, body: content)
    |> handle_response()
    |> dispatch_messages(team)
  end

  defp build_req(team) do
    Req.new(base_url: Livebook.Config.teams_url())
    |> Req.Request.put_new_header("x-lb-version", Livebook.Config.app_version())
    |> Livebook.Utils.req_attach_defaults()
    |> add_team_auth(team)
  end

  defp add_team_auth(req, nil), do: req

  defp add_team_auth(req, %{offline: %{}}) do
    Req.Request.append_request_steps(req, unauthorized: &{&1, Req.Response.new(status: 401)})
  end

  defp add_team_auth(req, %{session_token: @deploy_key_prefix <> _} = team) do
    token = "#{team.session_token}:#{Teams.Org.key_hash(%Teams.Org{teams_key: team.teams_key})}"
    Req.Request.merge_options(req, auth: {:bearer, token})
  end

  defp add_team_auth(req, %{user_id: nil} = team) do
    agent_name = Livebook.Config.agent_name()
    token = "#{team.session_token}:#{agent_name}:#{team.org_id}:#{team.org_key_id}"

    Req.Request.merge_options(req, auth: {:bearer, token})
  end

  defp add_team_auth(req, team) do
    token = "#{team.user_id}:#{team.org_id}:#{team.org_key_id}:#{team.session_token}"
    Req.Request.merge_options(req, auth: {:bearer, token})
  end

  defp handle_response(response) do
    case response do
      {:ok, %{status: status} = response} when status in 200..299 -> {:ok, response.body}
      {:ok, %{status: status} = response} when status in [410, 422] -> return_error(response)
      {:ok, %{status: 401}} -> {:transport_error, @unauthorized_error_message}
      _otherwise -> {:transport_error, @error_message}
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

  defp return_error(response) do
    if json?(response),
      do: {:error, response.body},
      else: {:transport_error, response.body}
  end

  defp json?(response) do
    "application/json; charset=utf-8" in Req.Response.get_header(response, "content-type")
  end
end

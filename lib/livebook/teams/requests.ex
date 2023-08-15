defmodule Livebook.Teams.Requests do
  @moduledoc false

  alias Livebook.Hubs.Team
  alias Livebook.Secrets.Secret
  alias Livebook.Teams
  alias Livebook.Teams.Org
  alias Livebook.Utils.HTTP

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
    headers = auth_headers(team)
    post("/api/v1/org/sign", %{payload: payload}, headers)
  end

  @doc """
  Send a request to Livebook Team API to create a secret.
  """
  @spec create_secret(Team.t(), Secret.t()) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def create_secret(team, secret) do
    {secret_key, sign_secret} = Teams.derive_keys(team.teams_key)
    secret_value = Teams.encrypt(secret.value, secret_key, sign_secret)

    headers = auth_headers(team)
    params = %{name: secret.name, value: secret_value}

    post("/api/v1/org/secrets", params, headers)
  end

  @doc """
  Send a request to Livebook Team API to update a secret.
  """
  @spec update_secret(Team.t(), Secret.t()) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def update_secret(team, secret) do
    {secret_key, sign_secret} = Teams.derive_keys(team.teams_key)
    secret_value = Teams.encrypt(secret.value, secret_key, sign_secret)

    headers = auth_headers(team)
    params = %{name: secret.name, value: secret_value}

    put("/api/v1/org/secrets", params, headers)
  end

  @doc """
  Send a request to Livebook Team API to delete a secret.
  """
  @spec delete_secret(Team.t(), Secret.t()) ::
          {:ok, String.t()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def delete_secret(team, secret) do
    headers = auth_headers(team)
    params = %{name: secret.name}

    delete("/api/v1/org/secrets", params, headers)
  end

  defp auth_headers(team) do
    token = "#{team.user_id}:#{team.org_id}:#{team.org_key_id}:#{team.session_token}"

    [
      {"x-lb-version", to_string(Application.spec(:livebook, :vsn))},
      {"authorization", "Bearer " <> token}
    ]
  end

  defp post(path, json, headers \\ []) do
    body = {"application/json", Jason.encode!(json)}
    request(:post, path, body: body, headers: headers)
  end

  defp put(path, json, headers) do
    body = {"application/json", Jason.encode!(json)}
    request(:put, path, body: body, headers: headers)
  end

  defp delete(path, json, headers) do
    body = {"application/json", Jason.encode!(json)}
    request(:delete, path, body: body, headers: headers)
  end

  defp get(path, params \\ %{}, headers \\ []) do
    query_string = URI.encode_query(params)
    path = if query_string != "", do: "#{path}?#{query_string}", else: path

    request(:get, path, headers: headers)
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

      _otherwise ->
        {:transport_error,
         "Something went wrong, try again later or please file a bug if it persists"}
    end
  end

  defp json?(headers) do
    HTTP.fetch_content_type(headers) == {:ok, "application/json"}
  end
end

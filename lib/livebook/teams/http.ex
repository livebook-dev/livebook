defmodule Livebook.Teams.HTTP do
  @moduledoc false

  alias Livebook.Teams.Org
  alias Livebook.Hubs.Team
  alias Livebook.Utils.HTTP

  @doc """
  Send a request to Livebook Team API to create a new org.
  """
  @spec create_org(Org.t()) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def create_org(org) do
    post("/api/org-request", %{name: org.name, key_hash: Org.key_hash(org)})
  end

  @doc """
  Send a request to Livebook Team API to join an org.
  """
  @spec join_org(Org.t()) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def join_org(org) do
    post("/api/org-request/join", %{name: org.name, key_hash: Org.key_hash(org)})
  end

  @doc """
  Send a request to Livebook Team API to get an org request.
  """
  @spec get_org_request_completion_data(pos_integer(), binary) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def get_org_request_completion_data(id, device_code) do
    get("/api/org-request/#{id}?device_code=#{device_code}")
  end

  @doc """
  Send a request to Livebook Team API to sign the given payload.
  """
  @spec org_sign(Team.t(), String.t()) ::
          {:ok, map()} | {:error, map() | String.t()} | {:transport_error, String.t()}
  def org_sign(team, payload) do
    headers = auth_headers(team)
    post("/api/org/sign", %{payload: payload}, headers)
  end

  defp auth_headers(team) do
    token = "#{team.user_id}:#{team.org_id}:#{team.org_key_id}:#{team.session_token}"
    [{"authorization", "Bearer " <> token}]
  end

  defp post(path, json, headers \\ []) do
    body = {"application/json", Jason.encode!(json)}
    request(:post, path, body: body, headers: headers)
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
      {:ok, status, header, body} when status in 200..299 ->
        if json?(header),
          do: {:ok, Jason.decode!(body)},
          else: {:error, body}

      {:ok, status, header, body} when status in [410, 422] ->
        if json?(header),
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

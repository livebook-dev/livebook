defmodule Livebook.Teams.Client do
  @moduledoc false

  alias Livebook.Teams.Org
  alias Livebook.Utils.HTTP

  @doc """
  Send a request to Livebook Team API to create a new org.
  """
  @spec create_org(Org.t()) ::
          {:ok, map()} | {:error, map()} | {:transport_error, :httpc.posix() | String.t()}
  def create_org(org) do
    post("/api/org-request", %{name: org.name, teams_key: org.teams_key})
  end

  @doc """
  Send a request to Livebook Team API to get an org request.
  """
  @spec get_org_request_completion_data(pos_integer()) ::
          {:ok, map()} | {:error, map()} | {:transport_error, :httpc.posix() | String.t()}
  def get_org_request_completion_data(id) do
    get("/api/org-request/#{id}")
  end

  defp post(path, json, header \\ []) do
    body = {"application/json", Jason.encode!(json)}
    request(:post, path, body: body, header: header)
  end

  defp get(path, params \\ %{}, header \\ []) do
    query_string = URI.encode_query(params)
    path = if query_string != "", do: "#{path}?#{query_string}", else: path

    request(:get, path, header: header)
  end

  defp request(method, path, opts) do
    endpoint = Livebook.Config.teams_url()
    url = endpoint <> path

    case HTTP.request(method, url, opts) do
      {:ok, status, _, body} when status in 200..299 ->
        {:ok, Jason.decode!(body)}

      {:ok, _, _, body} ->
        case Jason.decode(body) do
          {:ok, decoded_body} -> {:error, decoded_body}
          {:error, _exception} -> {:transport_error, body}
        end

      {:error, reason} ->
        {:transport_error, reason}
    end
  end
end

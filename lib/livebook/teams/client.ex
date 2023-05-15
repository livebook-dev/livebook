defmodule Livebook.Teams.Client do
  @moduledoc false

  alias Livebook.Teams.Org
  alias Livebook.Utils.HTTP

  @doc """
  Send a request to Livebook Team API to create a new org.
  """
  @spec create_org(Org.t()) :: {:ok, map()} | {:error, map()} | {:transport_error, String.t()}
  def create_org(org) do
    hash = :crypto.hash(:sha256, org.teams_key)
    key_hash = Base.url_encode64(hash)

    post("/api/org-request", %{name: org.name, key_hash: key_hash})
  end

  @doc """
  Send a request to Livebook Team API to get an org request.
  """
  @spec get_org_request_completion_data(pos_integer()) ::
          {:ok, map()} | {:error, map()} | {:transport_error, String.t()}
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
         "Well, this is embarrassing... An error has occurred and we're working to fix the problem!"}
    end
  end

  defp json?(header) do
    for {"content-type", value} <- header,
        value =~ "application/json",
        reduce: false,
        do: (_ -> true)
  end
end

defmodule Livebook.Teams.Client do
  @moduledoc false

  alias Livebook.Teams.Org
  alias Livebook.Utils.HTTP

  @doc """
  Send a request to Livebook Team API to create a new org.
  """
  @spec create_org(Org.t()) :: {:ok, map()} | {:error, String.t() | map() | atom()}
  def create_org(org) do
    post("/api/org-request", %{name: org.name, key_hash: org.teams_key})
  end

  @doc """
  Send a request to Livebook Team API to get an org request.
  """
  @spec get_org_request_completion_data(pos_integer()) ::
          {:ok, map()} | {:error, String.t() | map() | atom()}
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

    HTTP.request(method, url, opts)
    |> parse_json()
    |> handle_response()
  end

  defp parse_json({:ok, 200, _header, body}), do: {:ok, Jason.decode!(body)}
  defp parse_json({:ok, _status, _header, body}), do: {:error, Jason.decode!(body)}
  defp parse_json({:error, _} = error), do: error

  defp handle_response({:ok, _} = result), do: result
  defp handle_response({:error, %{"errors" => %{"detail" => reason}}}), do: {:error, reason}
  defp handle_response({:error, %{"errors" => errors}}), do: {:error, normalize_errors(errors)}
  defp handle_response({:error, _} = error), do: error

  defp normalize_errors(%{"key_hash" => errors} = response) do
    response
    |> Map.delete("key_hash")
    |> Map.put("teams_key", errors)
    |> normalize_errors()
  end

  defp normalize_errors(response) do
    for {key, errors} <- response, into: %{} do
      {String.to_existing_atom(key), errors}
    end
  end
end

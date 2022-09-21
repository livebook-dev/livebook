defmodule Livebook.Hubs.EnterpriseClient do
  @moduledoc false

  alias Livebook.Utils.HTTP

  @path "api/v1"

  def fetch_info(%{"url" => url, "token" => access_token}) do
    query = """
    query {
      info {
        id
        expire_at
      }
    }
    """

    with {:ok, %{"info" => info}} <- graphql(url, access_token, query) do
      {:ok, info}
    end
  end

  def fetch_me(%{"url" => url, "token" => access_token}) do
    query = """
    query {
      me {
        id
      }
    }
    """

    with {:ok, %{"me" => me}} <- graphql(url, access_token, query) do
      {:ok, me}
    end
  end

  defp graphql(url, access_token, query, input \\ %{}) do
    headers = [{"Authorization", "Bearer #{access_token}"}]
    body = {"application/json", Jason.encode!(%{query: query, variables: input})}

    case HTTP.request(:post, graphql_endpoint(url), headers: headers, body: body) do
      {:ok, 200, _, body} ->
        case Jason.decode!(body) do
          %{"errors" => [%{"message" => message}]} ->
            {:error, message}

          %{"data" => data} ->
            {:ok, data}
        end

      {:ok, _, _, body} ->
        {:error, body}

      {:error, _} = error ->
        error
    end
  end

  defp graphql_endpoint(url) do
    if String.ends_with?(url, "/"),
      do: "#{url}#{@path}",
      else: "#{url}/#{@path}"
  end
end

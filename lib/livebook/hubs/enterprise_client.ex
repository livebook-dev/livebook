defmodule Livebook.Hubs.EnterpriseClient do
  @moduledoc false

  alias Livebook.Utils.HTTP

  @path "/api/v1"

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
          %{"errors" => [%{"message" => "invalid_token"}]} ->
            {:error, "request failed with invalid token", :invalid_token}

          %{"errors" => [%{"message" => "unauthorized"}]} ->
            {:error, "request failed with unauthorized", :unauthorized}

          %{"errors" => [%{"message" => message}]} ->
            {:error, "request failed with message: #{message}", :other}

          %{"data" => data} ->
            {:ok, data}
        end

      {:error, {:failed_connect, _}} ->
        {:error, "request failed to connect", :invalid_url}
    end
  end

  defp graphql_endpoint(url), do: url <> @path
end

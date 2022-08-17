defmodule Livebook.Hubs.FlyClient do
  @moduledoc false

  alias Livebook.Hubs.Fly
  alias Livebook.Utils.HTTP

  def fetch_apps(access_token) do
    query = """
    query {
      apps {
        nodes {
          id
          organization {
            id
            name
            type
          }
        }
      }
    }
    """

    with {:ok, body} <- graphql(access_token, query) do
      apps =
        for node <- body["apps"]["nodes"] do
          %Fly{
            id: "fly-" <> node["id"],
            access_token: access_token,
            organization_id: node["organization"]["id"],
            organization_type: node["organization"]["type"],
            organization_name: node["organization"]["name"],
            application_id: node["id"]
          }
        end

      {:ok, apps}
    end
  end

  defp graphql(access_token, query) do
    headers = [{"Authorization", "Bearer #{access_token}"}]
    body = {"application/json", Jason.encode!(%{query: query})}

    case HTTP.request(:post, graphql_endpoint(), headers: headers, body: body) do
      {:ok, 200, _, body} ->
        case Jason.decode!(body) do
          %{"errors" => [%{"extensions" => %{"code" => code}}]} ->
            {:error, "request failed with code: #{code}"}

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

  defp graphql_endpoint do
    Application.get_env(:livebook, :fly_graphql_endpoint, "https://api.fly.io/graphql")
  end
end

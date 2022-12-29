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

    with {:ok, %{"apps" => %{"nodes" => nodes}}} <- graphql(access_token, query) do
      apps =
        for node <- nodes do
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

  def fetch_app(%Fly{application_id: app_id, access_token: access_token}) do
    query = """
    query($appId: String!) {
      app(id: $appId) {
        id
        name
        hostname
        platformVersion
        deployed
        status
        secrets {
          id
          name
          digest
          createdAt
        }
      }
    }
    """

    with {:ok, %{"app" => app}} <- graphql(access_token, query, %{appId: app_id}) do
      {:ok, app}
    end
  end

  def set_secrets(%Fly{access_token: access_token, application_id: application_id}, secrets) do
    mutation = """
    mutation($input: SetSecretsInput!) {
      setSecrets(input: $input) {
        app {
          secrets {
            id
            name
            digest
            createdAt
          }
        }
      }
    }
    """

    input = %{input: %{appId: application_id, secrets: secrets}}

    with {:ok, %{"setSecrets" => %{"app" => app}}} <- graphql(access_token, mutation, input) do
      {:ok, app["secrets"]}
    end
  end

  def unset_secrets(%Fly{access_token: access_token, application_id: application_id}, keys) do
    mutation = """
    mutation($input: UnsetSecretsInput!) {
      unsetSecrets(input: $input) {
        app {
          secrets {
            id
            name
            digest
            createdAt
          }
        }
      }
    }
    """

    input = %{input: %{appId: application_id, keys: keys}}

    with {:ok, %{"unsetSecrets" => %{"app" => app}}} <- graphql(access_token, mutation, input) do
      {:ok, app["secrets"]}
    end
  end

  defp graphql(access_token, query, input \\ %{}) do
    headers = [{"Authorization", "Bearer #{access_token}"}]
    body = {"application/json", Jason.encode!(%{query: query, variables: input})}

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

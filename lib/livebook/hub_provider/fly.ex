defmodule Livebook.HubProvider.Fly do
  @moduledoc false

  defstruct [:id, :name, :deployed, :hostname, :organization, :image_details, :state, :token]

  alias Livebook.Utils.HTTP

  def fetch_applications(access_token) do
    headers = [{"Authorization", "Bearer #{access_token}"}]
    body = {"application/json", Jason.encode!(%{query: query()})}

    with {:ok, body} <- request(headers, body) do
      {:ok, decode_nodes(body, access_token)}
    end
  end

  defp decode_nodes(%{"apps" => %{"nodes" => nodes}}, token) do
    for node <- nodes do
      org = node["organization"]
      image = node["latestImageDetails"]

      %__MODULE__{
        id: node["id"],
        name: node["name"],
        deployed: node["deployed"],
        hostname: node["hostname"],
        organization: %{
          id: org["id"],
          name: org["name"],
          slug: org["slug"],
          type: org["type"]
        },
        image_details: %{
          repository: image["repository"],
          registry: image["registry"],
          tag: image["tag"]
        },
        state: node["state"],
        token: token
      }
    end
  end

  defp graphql_endpoint do
    Application.get_env(:livebook, :fly_io_graphql_endpoint, "https://api.fly.io/graphql")
  end

  defp request(method \\ :post, url \\ graphql_endpoint(), headers, body) do
    HTTP.request(method, url, headers: headers, body: body)
    |> decode_body()
    |> handle_response()
  end

  defp decode_body({:ok, 200, _, body}), do: {:ok, Jason.decode!(body)}
  defp decode_body({:ok, _, _, body}), do: {:error, Jason.decode!(body)}
  defp decode_body({:error, _} = error), do: error

  defp handle_response({:ok, %{"errors" => [error]}}) do
    case error do
      %{"extensions" => %{"code" => code}} ->
        {:error, "request failed with code: #{code}"}

      %{"message" => message} ->
        {:error, message}
    end
  end

  defp handle_response({:ok, %{"data" => data}}), do: {:ok, data}
  defp handle_response(result), do: result

  defp query do
    """
    query {
      apps {
        nodes {
          id
          name
          deployed
          hostname
          organization {
            id
            slug
            name
            type
          }
          latestImageDetails {
            repository
            registry
            tag
          }
          state
        }
      }
    }
    """
  end
end

defimpl Livebook.HubProvider, for: Livebook.HubProvider.Fly do
  def fetch_hubs(%Livebook.HubProvider.Fly{token: token}) do
    case Livebook.HubProvider.Fly.fetch_applications(token) do
      {:ok, applications} ->
        hubs =
          for fly <- applications do
            %Livebook.HubProvider.Hub{
              id: fly.organization.id,
              type: "fly",
              label: fly.organization.name,
              token: fly.token
            }
          end

        {:ok, hubs}

      {:error, _} = error ->
        error
    end
  end
end

defmodule Livebook.Hub.Fly do
  @moduledoc false
  defstruct id: nil,
            name: nil,
            deployed: nil,
            hostname: nil,
            organization: nil,
            image_details: nil,
            state: nil,
            token: nil

  alias Livebook.Utils.HTTP

  @doc false
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
        reason =
          code
          |> Macro.underscore()
          |> String.to_atom()

        {:error, reason}

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

defimpl Livebook.Hub, for: Livebook.Hub.Fly do
  alias Livebook.Hub.{Fly, Machine}

  def fetch_machines(%Fly{token: token}) do
    case Fly.fetch_applications(token) do
      {:ok, applications} ->
        machines = for app <- applications, do: to_machine(app)
        {:ok, machines}

      {:error, _} = error ->
        error
    end
  end

  defp to_machine(%Fly{} = fly) do
    %Machine{
      id: fly.id,
      name: "#{fly.organization.name} - #{fly.name}",
      token: fly.token
    }
  end
end

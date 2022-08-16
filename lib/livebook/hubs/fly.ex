defmodule Livebook.Hubs.Fly do
  @moduledoc false
  defstruct [:id, :token, :name, :color, :organization]

  defmodule Organization do
    @moduledoc false
    defstruct [:id, :slug, :name, :type]

    @type t :: %__MODULE__{
            id: String.t(),
            slug: String.t(),
            name: String.t(),
            type: String.t()
          }
  end

  @type t :: %__MODULE__{
          id: Livebook.Utils.id(),
          token: String.t(),
          name: String.t(),
          color: Livebook.Users.User.hex_color(),
          organization: Organization.t()
        }

  alias Livebook.Utils.HTTP

  def fetch_organizations(access_token) do
    headers = [{"Authorization", "Bearer #{access_token}"}]

    query = """
    query {
      organizations {
        nodes {
          id
          slug
          name
          type
        }
      }
    }
    """

    body = {"application/json", Jason.encode!(%{query: query})}

    with {:ok, body} <- request(headers, body) do
      organizations =
        for node <- body["organizations"]["nodes"] do
          %Organization{
            id: node["id"],
            slug: node["slug"],
            name: node["name"],
            type: node["type"]
          }
        end

      {:ok, organizations}
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
end

defimpl Livebook.Hubs.HubProvider, for: Livebook.Hubs.Fly do
  def to_hub(%Livebook.Hubs.Fly{} = fly) do
    %Livebook.Hubs.Hub{
      id: fly.id,
      type: "fly",
      name: fly.name,
      provider: fly.organization,
      color: fly.color
    }
  end
end

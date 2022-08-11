defmodule Livebook.Hubs.Fly do
  @moduledoc false
  defstruct [:token]

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
      {:ok, body["organizations"]["nodes"]}
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
  def fetch_hubs(%Livebook.Hubs.Fly{token: token}) do
    case Livebook.Hubs.Fly.fetch_organizations(token) do
      {:ok, organizations} ->
        hubs =
          for organization <- organizations do
            %Livebook.Hubs.Hub{
              id: organization["id"],
              type: "fly",
              label: organization["name"],
              token: token
            }
          end

        {:ok, hubs}

      {:error, _} = error ->
        error
    end
  end
end

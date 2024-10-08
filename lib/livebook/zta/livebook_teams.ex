defmodule Livebook.ZTA.LivebookTeams do
  use GenServer
  use LivebookWeb, :verified_routes

  require Logger

  alias Livebook.Teams

  import Plug.Conn
  import Phoenix.Controller

  @behaviour Livebook.ZTA

  @impl true
  def child_spec(opts) do
    %{id: __MODULE__, start: {__MODULE__, :start_link, [opts]}}
  end

  def start_link(opts) do
    name = Keyword.fetch!(opts, :name)
    %{id: "team-" <> _} = team = Livebook.Hubs.get_default_hub()

    Livebook.ZTA.put(name, team)
    :ignore
  end

  @impl true
  def init(arg) do
    {:ok, arg}
  end

  @impl true
  def authenticate(name, %{params: %{"auth" => "true", "code" => code}} = conn, _opts) do
    team = Livebook.ZTA.get(name)

    with {:ok, access_token} <- retrieve_access_token(team, code),
         {:ok, metadata} <- get_user_info(team, access_token) do
      {conn
       |> put_session(:identity_data, metadata)
       |> redirect(to: conn.request_path)
       |> halt(), metadata}
    else
      _ -> {conn, nil}
    end
  end

  def authenticate(name, conn, _opts) do
    team = Livebook.ZTA.get(name)

    with %{payload: %{"access_token" => access_token}} <- get_session(conn, :identity_data),
         {:ok, metadata} <- get_user_info(team, access_token) do
      {conn, metadata}
    else
      _ -> request_user_authentication(conn, team)
    end
  end

  defp retrieve_access_token(team, code) do
    with {:ok, %{"access_token" => access_token}} <-
           Teams.Requests.retrieve_access_token(team, code) do
      {:ok, access_token}
    end
  end

  defp request_user_authentication(conn, team) do
    case Teams.Requests.request_user_authentication(team) do
      {:ok, %{"authorize_uri" => authorize_uri}} ->
        current_url = LivebookWeb.Endpoint.url() <> conn.request_path

        url =
          URI.parse(authorize_uri)
          |> URI.append_query("redirect_to=#{current_url}")
          |> URI.to_string()

        {conn |> redirect(external: url) |> halt(), nil}

      _ ->
        {conn
         |> put_status(400)
         |> halt(), nil}
    end
  end

  defp get_user_info(team, access_token) do
    with {:ok, payload} <- Teams.Requests.get_user_info(team, access_token) do
      %{"id" => id, "name" => name, "email" => email, "avatar_url" => avatar_url} = payload

      metadata = %{
        id: id,
        name: name,
        avatar_url: avatar_url,
        email: email,
        payload: %{"access_token" => access_token}
      }

      {:ok, metadata}
    end
  end
end

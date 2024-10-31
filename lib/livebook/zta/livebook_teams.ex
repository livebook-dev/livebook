defmodule Livebook.ZTA.LivebookTeams do
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
    identity_key = Keyword.fetch!(opts, :identity_key)
    team = Livebook.Hubs.fetch_hub!(identity_key)

    Livebook.ZTA.put(name, team)
    :ignore
  end

  @impl true
  def authenticate(name, conn, _opts) do
    team = Livebook.ZTA.get(name)

    if Livebook.Hubs.TeamClient.identity_enabled?(team.id) do
      handle_request(conn, team, conn.params)
    else
      {conn, nil}
    end
  end

  defp handle_request(conn, team, %{"teams_identity" => _, "code" => code}) do
    with {:ok, access_token} <- retrieve_access_token(team, code),
         {:ok, metadata} <- get_user_info(team, access_token) do
      {conn
       |> put_session(:identity_data, metadata)
       |> redirect(to: conn.request_path)
       |> halt(), metadata}
    else
      _ ->
        {conn
         |> redirect(to: conn.request_path)
         |> put_session(:teams_error, true)
         |> halt(), nil}
    end
  end

  defp handle_request(conn, _team, %{"teams_identity" => _, "failed_reason" => reason}) do
    {conn
     |> redirect(to: conn.request_path)
     |> put_session(:teams_failed_reason, reason)
     |> halt(), nil}
  end

  defp handle_request(conn, team, _params) do
    case get_session(conn) do
      %{"identity_data" => %{payload: %{"access_token" => access_token}}} ->
        validate_access_token(conn, team, access_token)

      # it means, we couldn't reach to Teams server
      %{"teams_error" => true} ->
        {conn
         |> delete_session(:teams_error)
         |> put_view(LivebookWeb.ErrorHTML)
         |> render("400.html", %{status: 400})
         |> halt(), nil}

      %{"teams_failed_reason" => reason} ->
        {conn
         |> delete_session(:teams_failed_reason)
         |> put_view(LivebookWeb.ErrorHTML)
         |> render("error.html", %{
           status: 403,
           details: "Failed to authenticate with Livebook Teams: #{reason}"
         })
         |> halt(), nil}

      _ ->
        request_user_authentication(conn, team)
    end
  end

  defp validate_access_token(conn, team, access_token) do
    case get_user_info(team, access_token) do
      {:ok, metadata} -> {conn, metadata}
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
    case Teams.Requests.create_auth_request(team) do
      {:ok, %{"authorize_uri" => authorize_uri}} ->
        current_url = LivebookWeb.Endpoint.url() <> conn.request_path <> "?teams_identity"

        url =
          URI.parse(authorize_uri)
          |> URI.append_query("redirect_to=#{URI.encode_www_form(current_url)}")
          |> URI.to_string()

        {conn |> redirect(external: url) |> halt(), nil}

      _ ->
        {conn
         |> redirect(to: conn.request_path)
         |> put_session(:teams_error, true)
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

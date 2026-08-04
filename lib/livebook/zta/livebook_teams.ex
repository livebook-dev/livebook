defmodule Livebook.ZTA.LivebookTeams do
  use LivebookWeb, :verified_routes

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

    case Livebook.Hubs.TeamClient.identity_status(team.id) do
      :enabled ->
        handle_request(conn, team, conn.params)

      :disabled ->
        {conn, %{}}

      :pending ->
        {conn
         |> put_status(:service_unavailable)
         |> put_view(LivebookWeb.ErrorHTML)
         |> render("error.html", %{
           status: 503,
           details:
             "This Livebook instance cannot be accessed because it has not yet" <>
               " established a connection to Livebook Teams."
         })
         |> halt(), nil}
    end
  end

  # Our extension to Livebook.ZTA to deal with logouts
  def logout(name, conn) do
    token = get_session(conn, :livebook_teams_access_token)
    team = Livebook.ZTA.get(name)

    url =
      Livebook.Config.teams_url()
      |> URI.new!()
      |> URI.append_path("/identity/logout")
      |> URI.append_query("org_id=#{team.org_id}&access_token=#{token}")
      |> URI.to_string()

    conn
    |> configure_session(renew: true)
    |> clear_session()
    |> redirect(external: url)
  end

  defp handle_request(conn, team, %{"teams_identity" => _} = params) do
    if valid_auth_state?(conn, params) do
      conn = delete_session(conn, :teams_auth_state)
      handle_identity_callback(conn, team, params)
    else
      restart_user_authentication(conn)
    end
  end

  defp handle_request(conn, team, %{"teams_redirect" => _, "redirect_to" => redirect_to}) do
    case Teams.Requests.create_auth_request(team) do
      {:ok, %{"authorize_uri" => authorize_uri}} ->
        uri =
          authorize_uri
          |> URI.new!()
          |> URI.append_query(URI.encode_query(%{"redirect_to" => redirect_to}))

        {conn
         |> redirect(external: URI.to_string(uri))
         |> halt(), nil}

      {_error_or_transport_error, _reason} ->
        {conn
         |> put_session(:teams_error, true)
         |> redirect(to: conn.request_path)
         |> halt(), nil}
    end
  end

  defp handle_request(conn, team, _params) do
    case get_session(conn) do
      %{"livebook_teams_access_token" => access_token} ->
        validate_access_token(conn, team, access_token)

      # it means, we couldn't reach to Teams server
      %{"teams_error" => true} ->
        {conn
         |> put_status(:bad_request)
         |> delete_session(:teams_error)
         |> put_view(LivebookWeb.ErrorHTML)
         |> render("400.html", %{status: 400})
         |> halt(), nil}

      %{"teams_failed_reason" => reason} ->
        {conn
         |> put_status(:forbidden)
         |> delete_session(:teams_failed_reason)
         |> put_view(LivebookWeb.ErrorHTML)
         |> render("error.html", %{
           status: 403,
           details: "Failed to authenticate with Livebook Teams: #{reason}"
         })
         |> halt(), nil}

      _ ->
        request_user_authentication(conn)
    end
  end

  defp handle_identity_callback(conn, team, %{"code" => code}) do
    with {:ok, access_token} <- retrieve_access_token(team, code),
         {:ok, metadata} <- get_user_info(team, access_token) do
      {conn
       |> put_session(:livebook_teams_access_token, access_token)
       |> redirect(to: conn.request_path)
       |> halt(), metadata}
    else
      _ ->
        {conn
         |> put_session(:teams_error, true)
         |> redirect(to: conn.request_path)
         |> halt(), nil}
    end
  end

  defp handle_identity_callback(conn, _team, %{"failed_reason" => reason}) do
    {conn
     |> put_session(:teams_failed_reason, reason)
     |> redirect(to: conn.request_path)
     |> halt(), nil}
  end

  defp handle_identity_callback(conn, _team, _params) do
    restart_user_authentication(conn)
  end

  defp validate_access_token(conn, team, access_token) do
    case get_user_info(team, access_token) do
      {:ok, metadata} -> {conn, metadata}
      _ -> request_user_authentication(conn)
    end
  end

  defp retrieve_access_token(team, code) do
    with {:ok, %{"access_token" => access_token}} <-
           Teams.Requests.retrieve_access_token(team, code) do
      {:ok, access_token}
    end
  end

  defp request_user_authentication(conn) do
    # The state binds the authentication flow to this browser session,
    # so that we only accept a code that we asked Livebook Teams for.
    # Otherwise anyone could get a code for their own identity and have
    # the browser complete the flow with it, effectively signing the
    # user into someone else's account.
    state = Livebook.Utils.random_long_id()

    # We have the browser do the redirect because the browser
    # knows the current page location. Unfortunately, it is quite
    # complex to know the actual host on the server, because the
    # user may be running inside a proxy. So in order to make the
    # feature more accessible, we do the redirecting on the client.
    html_document = """
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="UTF-8">
        <title>Redirecting...</title>
        <script>
          const redirectTo = new URL(window.location.href);
          redirectTo.searchParams.set("teams_identity", "");
          redirectTo.searchParams.set("teams_state", "#{state}");

          const url = new URL(window.location.href);
          url.searchParams.set("redirect_to", redirectTo.toString());
          url.searchParams.append("teams_redirect", "");

          window.location.href = url.toString();
        </script>
      </head>
    </html>
    """

    {conn |> put_session(:teams_auth_state, state) |> html(html_document) |> halt(), nil}
  end

  defp valid_auth_state?(conn, params) do
    with state when is_binary(state) <- get_session(conn, :teams_auth_state),
         param when is_binary(param) <- params["teams_state"] do
      Plug.Crypto.secure_compare(state, param)
    else
      _ -> false
    end
  end

  defp restart_user_authentication(conn) do
    # We redirect instead of rendering the authentication page right
    # away, so that the parameters of the stale callback are not
    # carried over into the new flow
    {conn |> redirect(to: conn.request_path) |> halt(), nil}
  end

  defp get_user_info(team, access_token) do
    with {:ok, payload} <- Teams.Requests.get_user_info(team, access_token) do
      {:ok, build_metadata(team.id, payload)}
    end
  end

  @doc """
  Returns the user metadata from given payload.
  """
  @spec build_metadata(String.t(), map()) :: Livebook.ZTA.metadata()
  def build_metadata(hub_id, payload) do
    %{
      "id" => id,
      "name" => name,
      "email" => email,
      "groups" => groups,
      "avatar_url" => avatar_url
    } = payload

    access_type =
      if Livebook.Hubs.TeamClient.user_full_access?(hub_id, groups),
        do: :full,
        else: :apps

    %{
      id: id,
      name: name,
      avatar_url: avatar_url,
      access_type: access_type,
      groups: groups,
      email: email,
      payload: payload
    }
  end
end

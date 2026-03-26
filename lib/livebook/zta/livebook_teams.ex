defmodule Livebook.ZTA.LivebookTeams do
  use LivebookWeb, :verified_routes

  alias Livebook.Teams

  import Plug.Conn
  import Phoenix.Controller

  @behaviour NimbleZTA

  @impl NimbleZTA
  def child_spec(opts) do
    %{id: __MODULE__, start: {__MODULE__, :start_link, [opts]}}
  end

  def start_link(opts) do
    name = Keyword.fetch!(opts, :name)
    id = Keyword.fetch!(opts, :identity_key)
    team = Livebook.Hubs.fetch_hub!(id)

    :ets.new(name, [:named_table, :public, :set, read_concurrency: true])

    NimbleZTA.put(name, team)
    :ignore
  end

  @impl NimbleZTA
  def authenticate(name, conn, _opts) do
    team = NimbleZTA.get(name)

    if Livebook.Hubs.TeamClient.identity_enabled?(team.id) do
      handle_request(name, conn, team, conn.params)
    else
      {conn, %{}}
    end
  end

  # Our extension to NimbleZTA to deal with logouts
  def logout(name, conn) do
    token = get_session(conn, :livebook_teams_access_token)
    team = NimbleZTA.get(name)

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

  defp handle_request(name, conn, team, %{"teams_identity" => _, "code" => code}) do
    with {:ok, access_token} <- retrieve_access_token(team, code),
         {:ok, payload} <- Teams.Requests.get_user_info(team, access_token) do
      metadata = build_metadata(team.id, payload)
      exp = System.os_time(:second) + 3 * 3600
      :ets.insert(name, {access_token, {exp, metadata}})

      {conn
       |> put_session(:livebook_teams_access_token, access_token)
       |> put_session(:livebook_teams_metadata_node, node())
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

  defp handle_request(_name, conn, _team, %{"teams_identity" => _, "failed_reason" => reason}) do
    {conn
     |> put_session(:teams_failed_reason, reason)
     |> redirect(to: conn.request_path)
     |> halt(), nil}
  end

  defp handle_request(_name, conn, team, %{"teams_redirect" => _, "redirect_to" => redirect_to}) do
    case Teams.Requests.create_auth_request(team) do
      {:ok, %{"authorize_uri" => authorize_uri}} ->
        uri =
          authorize_uri
          |> URI.new!()
          |> URI.append_query("redirect_to=#{redirect_to}")

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

  defp handle_request(name, conn, team, _params) do
    case get_session(conn) do
      %{"livebook_teams_access_token" => access_token} ->
        validate_access_token(name, conn, team, access_token)

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

  defp retrieve_access_token(team, code) do
    with {:ok, %{"access_token" => access_token}} <-
           Teams.Requests.retrieve_access_token(team, code) do
      {:ok, access_token}
    end
  end

  defp request_user_authentication(conn) do
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
          redirectTo.searchParams.append("teams_identity", "");

          const url = new URL(window.location.href);
          url.searchParams.set("redirect_to", redirectTo.toString());
          url.searchParams.append("teams_redirect", "");

          window.location.href = url.toString();
        </script>
      </head>
    </html>
    """

    {conn |> html(html_document) |> halt(), nil}
  end

  defp validate_access_token(name, conn, team, access_token) do
    node = get_session(conn, :livebook_teams_metadata_node)

    entry =
      try do
        :erpc.call(node, :ets, :lookup_element, [name, access_token, 2, nil])
      catch
        _, _ -> nil
      end

    case Teams.Requests.get_user_info(team, access_token) do
      {:ok, payload} ->
        {conn, build_metadata(team.id, payload)}

      :econnrefused ->
        case {System.os_time(:second), entry} do
          {current_timestamp, {exp, metadata}} when current_timestamp <= exp ->
            {conn, metadata}

          {_, entry} ->
            entry && :erpc.call(node, :ets, :delete, [name, access_token])

            {conn
             |> put_status(:service_unavailable)
             |> put_view(LivebookWeb.ErrorHTML)
             |> render("503.html")
             |> halt(), nil}
        end

      _otherwise ->
        entry && :erpc.call(node, :ets, :delete, [name, access_token])
        request_user_authentication(conn)
    end
  end

  @doc """
  Returns the user metadata from given payload.
  """
  @spec build_metadata(String.t(), map()) :: NimbleZTA.metadata()
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

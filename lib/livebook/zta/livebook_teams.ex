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

    # we need to guarantee the hub exists
    # otherwise, it should raise an exception
    _ = Livebook.Hubs.fetch_hub!(identity_key)
    Livebook.ZTA.put(name, identity_key)

    :ignore
  end

  @impl true
  def authenticate(name, conn, _opts) do
    id = Livebook.ZTA.get(name)
    # fetch the hub everytime, so we will have the `billing_status` updated
    team = Livebook.Hubs.fetch_hub!(id)

    if Livebook.Hubs.TeamClient.identity_enabled?(team.id) do
      if Livebook.Hubs.TeamClient.billing_good_standing?(team.id) do
        handle_request(conn, team, conn.params)
      else
        {conn
         |> put_status(:payment_required)
         |> put_view(LivebookWeb.ErrorHTML)
         |> put_root_layout(false)
         |> render("error.html", %{
           status: 402,
           title: billing_status_title(team.billing_status.type),
           details: billing_status_message(team.billing_status.type)
         })
         |> halt(), nil}
      end
    else
      {conn, %{}}
    end
  end

  # Our extension to Livebook.ZTA to deal with logouts
  def logout(name, conn) do
    token = get_session(conn, :livebook_teams_access_token)
    id = Livebook.ZTA.get(name)
    team = Livebook.Hubs.fetch_hub!(id)

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

  defp handle_request(conn, team, %{"teams_identity" => _, "code" => code}) do
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

  defp handle_request(conn, _team, %{"teams_identity" => _, "failed_reason" => reason}) do
    {conn
     |> put_session(:teams_failed_reason, reason)
     |> redirect(to: conn.request_path)
     |> halt(), nil}
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
         |> put_root_layout(false)
         |> render("400.html", %{status: 400})
         |> halt(), nil}

      %{"teams_failed_reason" => reason} ->
        {conn
         |> put_status(:forbidden)
         |> delete_session(:teams_failed_reason)
         |> put_view(LivebookWeb.ErrorHTML)
         |> put_root_layout(false)
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
        # We have the browser do the redirect because the browser
        # knows the current page location. Unfortunately, it is quite
        # complex to know the actual host on the server, because the
        # user may be running inside a proxy. So in order to make the
        # feature more accessible, we do the redirecting on the client.
        conn =
          html(conn, """
          <!DOCTYPE html>
          <html lang="en">
            <head>
              <meta charset="UTF-8">
              <title>Redirecting...</title>
              <script>
                const redirectTo = new URL(window.location.href);
                redirectTo.searchParams.append("teams_identity", "");

                const url = new URL("#{authorize_uri}");
                url.searchParams.set("redirect_to", redirectTo.toString());
                window.location.href = url.toString();
              </script>
            </head>
          </html>
          """)

        {halt(conn), nil}

      {:error, %{"errors" => %{"detail" => "Payment Required"}}} ->
        {conn
         |> put_status(:payment_required)
         |> put_view(LivebookWeb.ErrorHTML)
         |> put_root_layout(false)
         |> render("error.html", %{
           status: 402,
           title: billing_status_title(team.billing_status.type),
           details: billing_status_message(team.billing_status.type)
         })
         |> halt(), nil}

      _otherwise ->
        {conn
         |> put_session(:teams_error, true)
         |> redirect(to: conn.request_path)
         |> halt(), nil}
    end
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

  # TODO: this is just a placeholder, so the copy must be improved
  defp billing_status_title(:canceled), do: "Your plan has been canceled."
  defp billing_status_title(:trial_ended), do: "Your trial has ended."

  defp billing_status_message(:canceled) do
    "An admin of your organization needs to subscribe again to a paid plan."
  end

  defp billing_status_message(:trial_ended) do
    "An admin of your organization needs to subscribe to a paid plan."
  end
end

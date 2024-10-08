defmodule LivebookWeb.UserPlug do
  # Initializes the session and cookies with user-related info.
  #
  # The first time someone visits Livebook
  # this plug stores a new random user id or the ZTA user
  # in the session under `:identity_data`.
  #
  # Additionally the cookies are checked for the presence
  # of `"user_data"` and if there is none, a new user
  # attributes are stored there. This makes sure
  # the client-side can always access some `"user_data"`
  # for `connect_params` of the socket connection.

  @behaviour Plug

  import Plug.Conn
  import Phoenix.Controller

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    conn
    |> ensure_user_identity()
    |> ensure_user_data()
    |> mirror_user_data_in_session()
  end

  defp ensure_user_identity(conn) do
    {_type, module, _key} = Livebook.Config.identity_provider()
    {conn, identity_data} = module.authenticate(LivebookWeb.ZTA, conn, [])

    cond do
      conn.halted ->
        conn

      identity_data ->
        # Ensure we have a unique ID to identify this user/session.
        id =
          identity_data[:id] || get_session(conn, :identity_data)[:id] ||
            Livebook.Utils.random_long_id()

        put_session(conn, :identity_data, Map.put(identity_data, :id, id))

      true ->
        conn
        |> put_status(:forbidden)
        |> put_view(LivebookWeb.ErrorHTML)
        |> render("403.html", %{status: 403})
        |> halt()
    end
  end

  defp ensure_user_data(conn) when conn.halted, do: conn

  defp ensure_user_data(conn) do
    if Map.has_key?(conn.req_cookies, "lb_user_data") do
      conn
    else
      encoded =
        %{"name" => nil, "hex_color" => Livebook.EctoTypes.HexColor.random()}
        |> Jason.encode!()
        |> Base.encode64()

      # We disable HttpOnly, so that it can be accessed on the client
      # and set expiration to 5 years
      opts = [http_only: false, max_age: 157_680_000] ++ LivebookWeb.Endpoint.cookie_options()
      put_resp_cookie(conn, "lb_user_data", encoded, opts)
    end
  end

  # Copies user_data from cookie to session, so that it's
  # accessible to LiveViews
  defp mirror_user_data_in_session(conn) when conn.halted, do: conn

  defp mirror_user_data_in_session(conn) do
    user_data = conn.cookies["lb_user_data"] |> Base.decode64!() |> Jason.decode!()
    put_session(conn, :user_data, user_data)
  end
end

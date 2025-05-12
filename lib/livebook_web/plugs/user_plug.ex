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
    conn = ensure_user_identity(conn)

    if conn.halted do
      conn
    else
      conn
      |> ensure_user_data()
      |> assign_user_data()
      |> set_logger_metadata()
    end
  end

  defp ensure_user_identity(conn) do
    {_type, module, _key} = identity_provider(conn)
    {conn, identity_data} = module.authenticate(LivebookWeb.ZTA, conn, [])

    cond do
      conn.halted ->
        conn

      identity_data ->
        # Ensure we have a unique ID to identify this user/session.
        id = identity_data[:id] || get_session(conn, :user_id) || Livebook.Utils.random_long_id()

        conn
        |> assign(:identity_data, identity_data)
        |> put_session(:user_id, id)

      true ->
        conn
        |> put_status(:forbidden)
        |> put_view(LivebookWeb.ErrorHTML)
        |> render("403.html", %{status: 403})
        |> halt()
    end
  end

  defp ensure_user_data(conn) do
    if Map.has_key?(conn.req_cookies, "lb_user_data") do
      conn
    else
      encoded =
        %{"name" => nil, "hex_color" => Livebook.EctoTypes.HexColor.random()}
        |> JSON.encode!()
        |> Base.encode64()

      # We disable HttpOnly, so that it can be accessed on the client
      # and set expiration to 5 years
      opts = [http_only: false, max_age: 157_680_000] ++ LivebookWeb.Endpoint.cookie_options()
      put_resp_cookie(conn, "lb_user_data", encoded, opts)
    end
  end

  # Copies user_data from cookie to assigns, which we later copy into
  # LV session
  defp assign_user_data(conn) do
    user_data = conn.cookies["lb_user_data"] |> Base.decode64!() |> JSON.decode!()
    assign(conn, :user_data, user_data)
  end

  defp set_logger_metadata(conn) do
    session = get_session(conn)
    %{identity_data: identity_data, user_data: user_data} = conn.assigns
    current_user = build_current_user(session, identity_data, user_data)

    Logger.metadata(Livebook.Utils.logger_users_metadata([current_user]))
    conn
  end

  @doc """
  Builds `Livebook.Users.User` using information from connection and
  the session.

  We accept individual arguments, because this is used both in plug
  and LV hooks.
  """
  def build_current_user(%{} = session, %{} = identity_data, %{} = user_data) do
    identity_data = Map.new(identity_data, fn {k, v} -> {Atom.to_string(k), v} end)

    attrs =
      case Map.merge(user_data, identity_data) do
        %{"name" => nil, "email" => email} = attrs -> %{attrs | "name" => email}
        attrs -> attrs
      end

    user = Livebook.Users.User.new(session["user_id"])

    case Livebook.Users.update_user(user, attrs) do
      {:ok, user} -> user
      {:error, _changeset} -> user
    end
  end

  @doc """
  Returns fields to be merged into the LV session.
  """
  def extra_lv_session(conn) do
    # These attributes are always retrieved in UserPlug, so we don't
    # need to store them in the session. We need to pass them to LV,
    # so we copy the assigns into LV session. This is particularly
    # important for identity data, which can be huge and may exceed
    # cookie limit, if it was stored in the session.
    %{
      "identity_data" => conn.assigns.identity_data,
      "user_data" => conn.assigns.user_data
    }
  end

  @doc """
  Returns the identity provider configuration for the given `conn` or
  `session`.

  This mirrors `Livebook.Config.identity_provider/0`, except the it can
  be overridden in tests, for each connection.
  """
  @spec identity_provider(Plug.Conn.t() | map()) :: {atom(), module, binary}
  if Mix.env() == :test do
    def identity_provider(%Plug.Conn{} = conn), do: identity_provider(get_session(conn))

    def identity_provider(%{} = session) do
      session["identity_provider_test_override"] || Livebook.Config.identity_provider()
    end
  else
    def identity_provider(_), do: Livebook.Config.identity_provider()
  end
end

defmodule LivebookWeb.AuthPlug do
  @behaviour Plug

  import Plug.Conn
  import Phoenix.Controller

  use LivebookWeb, :verified_routes

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    if authenticated?(conn) do
      if not authorized?(conn) do
        render_unauthorized(conn)
      else
        conn
      end
    else
      authenticate(conn)
    end
  end

  @doc """
  Stores in the session the secret for the given mode.
  """
  @spec store(Plug.Conn.t(), Livebook.Config.authentication_mode(), String.t()) :: Plug.Conn.t()
  def store(conn, mode, value) do
    conn
    |> put_session(key(conn.port, mode), hash(value))
    |> configure_session(renew: true)
  end

  @doc """
  Checks if given connection is already authenticated.
  """
  @spec authenticated?(Plug.Conn.t()) :: boolean()
  def authenticated?(conn) do
    authenticated?(get_session(conn), conn.port)
  end

  defp authenticated?(session, port) do
    case authentication(session) do
      %{mode: :disabled} ->
        true

      %{mode: mode, secret: secret} when mode in [:token, :password] ->
        secret_hash = session[key(port, mode)]
        is_binary(secret_hash) and matches_secret?(secret_hash, secret)
    end
  end

  @doc """
  Checks if given connection or session is authorized.
  """
  @spec authorized?(Plug.Conn.t()) :: boolean()
  def authorized?(%Plug.Conn{} = conn) do
    # Note that if the user has access restricted to specific app pages,
    # they are not authorized and have no access to any pages guarded
    # by this plug.
    authenticated?(conn) and
      LivebookWeb.UserPlug.build_current_user(
        get_session(conn),
        conn.assigns.identity_data,
        conn.assigns.user_data
      ).restricted_apps_groups == nil
  end

  @doc """
  Checks if the given session is authorized.
  """
  @spec authorized?(map(), non_neg_integer()) :: boolean()
  def authorized?(%{} = session, port) do
    authenticated?(session, port) and
      LivebookWeb.UserPlug.build_current_user(
        session,
        session["identity_data"],
        session["user_data"]
      ).restricted_apps_groups == nil
  end

  defp authenticate(conn) do
    case authentication(conn) do
      %{mode: :password} ->
        redirect_to_authenticate(conn)

      %{mode: :token, secret: secret} ->
        {token, query_params} = Map.pop(conn.query_params, "token")

        if is_binary(token) and matches_secret?(hash(token), secret) do
          # Redirect to the same path without query params
          conn
          |> store(:token, token)
          |> redirect(to: path_with_query(conn.request_path, query_params))
          |> halt()
        else
          redirect_to_authenticate(conn)
        end
    end
  end

  defp matches_secret?(hash, secret) do
    Plug.Crypto.secure_compare(hash, hash(secret))
  end

  defp redirect_to_authenticate(%{path_info: []} = conn) do
    path =
      if Livebook.Apps.list_apps() != [] or Livebook.Config.apps_path() != nil or
           Livebook.Config.teams_auth() != nil do
        ~p"/apps"
      else
        ~p"/authenticate"
      end

    conn
    |> redirect(to: path)
    |> halt()
  end

  defp redirect_to_authenticate(conn) do
    conn
    |> then(fn
      %{method: "GET"} -> put_session(conn, :redirect_to, current_path(conn))
      conn -> conn
    end)
    |> redirect(to: ~p"/authenticate")
    |> halt()
  end

  defp render_unauthorized(%{path_info: []} = conn) do
    conn |> redirect(to: ~p"/apps") |> halt()
  end

  defp render_unauthorized(conn) do
    conn
    |> put_status(:unauthorized)
    |> put_view(LivebookWeb.ErrorHTML)
    |> render("401.html", %{details: "You don't have permission to access this server"})
    |> halt()
  end

  defp path_with_query(path, params) when params == %{}, do: path
  defp path_with_query(path, params), do: path <> "?" <> URI.encode_query(params)

  defp key(port, mode), do: "#{port}:#{mode}"
  defp hash(value), do: :crypto.hash(:sha256, value)

  @doc """
  Returns the authentication configuration for the given `conn` or
  `session`.

  This mirrors `Livebook.Config.authentication/0`, except the it can
  be overridden in tests, for each connection.
  """
  @spec authentication(Plug.Conn.t() | map()) :: Livebook.Config.authentication()
  if Mix.env() == :test do
    def authentication(%Plug.Conn{} = conn), do: authentication(get_session(conn))

    def authentication(%{} = session) do
      session["authentication_test_override"] || Livebook.Config.authentication()
    end
  else
    def authentication(_), do: Livebook.Config.authentication()
  end
end

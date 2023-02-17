defmodule LivebookWeb.AppAuthPlug do
  @moduledoc false

  @behaviour Plug

  import Plug.Conn
  import Phoenix.Controller

  alias LivebookWeb.Router.Helpers, as: Routes

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    slug = conn.path_params["slug"]

    case Livebook.Apps.fetch_settings_by_slug(slug) do
      {:ok, %{access_type: :public}} ->
        conn

      {:ok, %{access_type: :protected} = app_settings} ->
        if authenticated?(get_session(conn), conn.port, app_settings) do
          conn
        else
          conn
          |> redirect(to: Routes.app_auth_path(conn, :index, app_settings.slug))
          |> halt()
        end

      :error ->
        conn
        |> redirect(to: "/")
        |> halt()
    end
  end

  @doc """
  Stores app password in the session.
  """
  @spec store(Plug.Conn.t(), String.t()) :: Plug.Conn.t()
  def store(conn, password) do
    slug = conn.path_params["slug"]
    put_session(conn, key(conn.port, slug), hash(password))
  end

  @doc """
  Checks if given connection is already authenticated.
  """
  @spec authenticated?(Plug.Conn.t()) :: boolean()
  def authenticated?(conn) do
    LivebookWeb.AuthPlug.authenticated?(conn, Livebook.Config.auth_mode()) or
      app_authenticated?(conn)
  end

  defp app_authenticated?(conn) do
    slug = conn.path_params["slug"]

    case Livebook.Apps.fetch_settings_by_slug(slug) do
      {:ok, app_settings} ->
        authenticated?(get_session(conn), conn.port, app_settings)

      :error ->
        false
    end
  end

  @doc """
  Checks if the given session is authenticated.
  """
  @spec authenticated?(map(), non_neg_integer(), Livebook.Notebook.AppSettings.t()) :: boolean()
  def authenticated?(session, port, app_settings) do
    LivebookWeb.AuthPlug.authenticated?(session, port, Livebook.Config.auth_mode()) or
      app_authenticated?(session, port, app_settings)
  end

  defp app_authenticated?(session, port, app_settings) do
    secret = session[key(port, app_settings.slug)]
    is_binary(secret) and Plug.Crypto.secure_compare(secret, hash(app_settings.password))
  end

  defp key(port, slug), do: "#{port}:app_password:#{slug}"
  defp hash(value), do: :crypto.hash(:sha256, value)
end

defmodule LivebookWeb.ZTIPlug do
  @moduledoc false

  @behaviour Plug

  import Plug.Conn
  import Phoenix.Controller

  use LivebookWeb, :verified_routes

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    identity = Livebook.Config.zti()
    if verified?(conn, identity), do: conn, else: verify(conn, identity)
  end

  @doc """
  Stores in the session the key for the given zero trust identity.
  """
  @spec store(Plug.Conn.t(), Livebook.Config.zti(), String.t()) :: Plug.Conn.t()
  def store(conn, identity, value) do
    conn
    |> put_session(key(conn.port, identity), hash(value))
    |> configure_session(renew: true)
  end

  @doc """
  Checks if given connection is already verified.
  """
  @spec verified?(Plug.Conn.t(), Livebook.Config.zti()) :: boolean()
  def verified?(conn, identity) do
    verified?(get_session(conn), conn.port, identity)
  end

  @doc """
  Checks if the given session is verified.
  """
  @spec verified?(map(), non_neg_integer(), Livebook.Config.zti()) :: boolean()
  def verified?(session, port, identity)

  def verified?(_session, _port, nil) do
    true
  end

  def verified?(session, port, identity) do
    secret = session[key(port, identity)]

    is_binary(secret) and identity == Livebook.Config.zti() and
      Plug.Crypto.secure_compare(secret, expected())
  end

  defp verify(conn, identity) do
    with [token] <- get_req_header(conn, "cf-access-jwt-assertion"),
         {:ok, token} <- verify_token(token),
         {:ok, iss} <- verify_iss(token) do
      conn
      |> store(identity, iss)
      |> redirect(to: path_with_query(conn.request_path, conn.query_params))
      |> halt()
    else
      _ ->
        conn
        |> put_status(:forbidden)
        |> put_view(LivebookWeb.ErrorHTML)
        |> render("403.html")
        |> halt()
    end
  end

  defp path_with_query(path, params) when params == %{}, do: path
  defp path_with_query(path, params), do: path <> "?" <> URI.encode_query(params)

  defp key(port, identity), do: "#{port}:#{identity}"
  defp expected(), do: hash(Application.fetch_env!(:livebook, :zti_key))
  defp hash(value), do: :crypto.hash(:sha256, value)

  defp verify_token(token) do
    keys = Livebook.ZTIKeys.get()

    Enum.find_value(keys, fn key ->
      case JOSE.JWT.verify(key, token) do
        {true, token, _s} -> {:ok, token}
        {_, _t, _s} -> nil
      end
    end)
  end

  defp verify_iss(%{fields: %{"iss" => "https://" <> domain}}) do
    iss = String.split(domain, ".") |> hd()
    if iss == Application.fetch_env!(:livebook, :zti_key), do: {:ok, iss}
  end
end

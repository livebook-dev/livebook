defmodule Livebook.ZTA.Teleport do
  use GenServer
  require Logger

  defstruct [:req_options, :jwks]

  @renew_afer 24 * 60 * 60 * 1000
  @fields %{"sub" => :id, "username" => :username}
  @assertion "teleport-jwt-assertion"
  @well_known_jwks_path "/.well-known/jwks.json"

  def start_link(opts) do
    url =
      opts[:identity_key]
      |> URI.parse()
      |> URI.append_path(@well_known_jwks_path)
      |> URI.to_string()

    options = [req_options: [url: url]]

    GenServer.start_link(__MODULE__, options, Keyword.take(opts, [:name]))
  end

  def authenticate(name, conn, _opts) do
    token = Plug.Conn.get_req_header(conn, @assertion)

    jwks = GenServer.call(name, :get_jwks, :infinity)

    {conn, authenticate_user(token, jwks)}
  end

  @impl true
  def init(options) do
    state = struct!(__MODULE__, options)

    {:ok, %{state | jwks: renew_jwks(state.req_options)}}
  end

  @impl true
  def handle_info(:renew_jwks, state) do
    {:noreply, %{state | jwks: renew_jwks(state.req_options)}}
  end

  @impl true
  def handle_call(:get_jwks, _, state) do
    {:reply, state.jwks, state}
  end

  defp authenticate_user(token, jwks) do
    with [encoded_token] <- token,
         {:ok, %{fields: %{"exp" => exp, "nbf" => nbf}} = token} <-
           verify_token(encoded_token, jwks),
         :ok <- verify_timestamps(exp, nbf) do
      for(
        {k, v} <- token.fields,
        new_k = @fields[k],
        do: {new_k, v},
        into: %{payload: token.fields}
      )
    else
      _ ->
        nil
    end
  end

  defp verify_token(token, keys) do
    Enum.find_value(keys, :error, fn key ->
      case JOSE.JWT.verify(key, token) do
        {true, token, _s} -> {:ok, token}
        _ -> nil
      end
    end)
  end

  defp verify_timestamps(exp, nbf) do
    now = DateTime.utc_now()

    with {:ok, exp} <- DateTime.from_unix(exp),
         {:ok, nbf} <- DateTime.from_unix(nbf),
         true <- DateTime.after?(exp, now),
         true <- DateTime.after?(now, nbf) do
      :ok
    else
      _ -> :error
    end
  end

  defp renew_jwks(req_options) do
    keys = Req.request!(req_options).body["keys"]

    jwks = JOSE.JWK.from_map(keys)

    Process.send_after(self(), :renew_jwks, @renew_afer)
    jwks
  end
end

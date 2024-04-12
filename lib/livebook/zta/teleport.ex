defmodule Livebook.ZTA.Teleport do
  use GenServer
  require Logger

  defstruct [:req_options, :name]

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

    name = Keyword.fetch!(opts, :name)
    options = [req_options: [url: url], name: name]
    GenServer.start_link(__MODULE__, options, name: name)
  end

  def authenticate(name, conn, _opts) do
    token = Plug.Conn.get_req_header(conn, @assertion)
    jwks = Livebook.ZTA.get(name)
    {conn, authenticate_user(token, jwks)}
  end

  @impl true
  def init(options) do
    state = struct!(__MODULE__, options)
    {:ok, renew(state)}
  end

  @impl true
  def handle_info(:renew, state) do
    {:noreply, renew(state)}
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

  defp renew(state) do
    keys = Req.request!(state.req_options).body["keys"]
    jwks = JOSE.JWK.from_map(keys)
    Process.send_after(self(), :renew, @renew_afer)
    Livebook.ZTA.put(state.name, jwks)
    state
  end
end

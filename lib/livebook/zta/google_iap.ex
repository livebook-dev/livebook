defmodule Livebook.ZTA.GoogleIAP do
  use GenServer
  require Logger
  import Plug.Conn

  @assertion "x-goog-iap-jwt-assertion"
  @renew_afer 24 * 60 * 60 * 1000
  @fields %{"sub" => :id, "name" => :name, "email" => :email}

  defstruct [:req_options, :identity, :keys]

  def start_link(opts) do
    identity = opts[:custom_identity] || identity(opts[:identity_key])
    options = [req_options: [url: identity.certs], identity: identity, keys: nil]
    GenServer.start_link(__MODULE__, options, Keyword.take(opts, [:name]))
  end

  def authenticate(name, conn, _opts) do
    token = get_req_header(conn, @assertion)
    {identity, keys} = GenServer.call(name, :info, :infinity)
    {conn, authenticate_user(token, identity, keys)}
  end

  @impl true
  def init(options) do
    state = struct!(__MODULE__, options)
    {:ok, %{state | keys: keys(state)}}
  end

  @impl true
  def handle_call(:info, _from, state) do
    {:reply, {state.identity, state.keys}, state}
  end

  @impl true
  def handle_info(:renew, state) do
    {:noreply, %{state | keys: keys(state)}}
  end

  defp keys(state) do
    Logger.debug("[#{inspect(__MODULE__)}] requesting #{inspect(state.req_options)}")
    keys = Req.request!(state.req_options).body["keys"]
    Process.send_after(self(), :renew, @renew_afer)
    keys
  end

  defp authenticate_user(token, identity, keys) do
    with [encoded_token] <- token,
         {:ok, token} <- verify_token(encoded_token, keys),
         :ok <- verify_iss(token, identity.iss, identity.key) do
      for({k, v} <- token.fields, new_k = @fields[k], do: {new_k, v}, into: %{})
    else
      _ -> nil
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

  defp verify_iss(%{fields: %{"iss" => iss, "aud" => key}}, iss, key), do: :ok
  defp verify_iss(_, _, _), do: :error

  defp identity(key) do
    %{
      key: key,
      key_type: "aud",
      iss: "https://cloud.google.com/iap",
      certs: "https://www.gstatic.com/iap/verify/public_key-jwk",
      assertion: "x-goog-iap-jwt-assertion",
      email: "x-goog-authenticated-user-email",
      user_identity: "https://www.googleapis.com/plus/v1/people/me"
    }
  end
end

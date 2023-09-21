defmodule Livebook.ZTA.Cloudflare do
  use GenServer
  require Logger
  import Plug.Conn

  @assertion "cf-access-jwt-assertion"
  @renew_afer 24 * 60 * 60 * 1000
  @fields %{"user_uuid" => :id, "name" => :name, "email" => :email}

  defstruct [:name, :req_options, :identity, :keys]

  def start_link(opts) do
    identity = opts[:custom_identity] || identity(opts[:identity][:key])
    options = [req_options: [url: identity.certs], identity: identity, keys: nil]
    GenServer.start_link(__MODULE__, options, name: opts[:name])
  end

  def authenticate(name, conn, fields: fields) do
    token = get_req_header(conn, @assertion)
    {identity, keys} = GenServer.call(name, :info, :infinity)
    {conn, authenticate_user(token, fields, identity, keys)}
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

  defp authenticate_user(token, _fields, identity, keys) do
    with [encoded_token] <- token,
         {:ok, token} <- verify_token(encoded_token, keys),
         :ok <- verify_iss(token, identity.iss),
         {:ok, user} <- get_user_identity(encoded_token, identity.user_identity) do
      for({k, v} <- user, new_k = @fields[k], do: {new_k, v}, into: %{})
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

  defp verify_iss(%{fields: %{"iss" => iss}}, iss), do: :ok
  defp verify_iss(_, _), do: :error

  defp get_user_identity(token, url) do
    token = "CF_Authorization=#{token}"
    resp = Req.request!(url: url, headers: [{"cookie", token}])
    if resp.status == 200, do: {:ok, resp.body}, else: :error
  end

  defp identity(key) do
    %{
      key: key,
      key_type: "domain",
      iss: "https://#{key}.cloudflareaccess.com",
      certs: "https://#{key}.cloudflareaccess.com/cdn-cgi/access/certs",
      assertion: "cf-access-jwt-assertion",
      email: "cf-access-authenticated-user-email",
      user_identity: "https://#{key}.cloudflareaccess.com/cdn-cgi/access/get-identity"
    }
  end
end

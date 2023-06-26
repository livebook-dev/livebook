defmodule Livebook.ZTA.Cloudflare do
  @moduledoc false

  use GenServer
  require Logger
  import Plug.Conn

  @assertion "cf-access-jwt-assertion"
  @renew_afer 24 * 60 * 60 * 1000

  defstruct [:name, :req_options, :identity]

  def start_link(opts) do
    identity = identity(opts[:identity][:key])
    options = [req_options: [url: identity.certs], identity: identity]
    GenServer.start_link(__MODULE__, options, name: opts[:name])
  end

  def authenticate(name, conn, fields: fields) do
    token = get_req_header(conn, @assertion)
    GenServer.call(name, {:authenticate, token, fields})
  end

  @impl true
  def init(options) do
    :ets.new(options[:name], [:public, :named_table])
    {:ok, struct!(__MODULE__, options)}
  end

  @impl true
  def handle_call(:get_keys, _from, state) do
    keys = get_from_ets(state.name) || request_and_store_in_ets(state)
    {:reply, keys, state}
  end

  def handle_call({:authenticate, token, fields}, _from, state) do
    keys = get_from_ets(state.name) || request_and_store_in_ets(state)
    user = authenticated_user(token, fields, state.identity, keys)
    {:reply, user, state}
  end

  @impl true
  def handle_info(:request, state) do
    request_and_store_in_ets(state)
    {:noreply, state}
  end

  defp request_and_store_in_ets(state) do
    Logger.debug("[#{inspect(__MODULE__)}] requesting #{inspect(state.req_options)}")
    keys = Req.request!(state.req_options).body["keys"]
    :ets.insert(state.name, keys: keys)
    Process.send_after(self(), :request, @renew_afer)
    keys
  end

  defp get_from_ets(name) do
    case :ets.lookup(name, :keys) do
      [keys: keys] -> keys
      [] -> nil
    end
  end

  defp authenticated_user(token, fields, identity, keys) do
    with [encoded_token] <- token,
         {:ok, token} <- verify_token(encoded_token, keys),
         :ok <- verify_iss(token, identity.iss),
         {:ok, user} <- get_user_identity(encoded_token, fields, identity.identity) do
      Map.new(user, fn {k, v} -> {String.to_atom(k), to_string(v)} end)
    else
      _ -> nil
    end
  end

  defp verify_token(token, keys) do
    Enum.find_value(keys, fn key ->
      case JOSE.JWT.verify(key, token) do
        {true, token, _s} -> {:ok, token}
        {_, _t, _s} -> nil
      end
    end)
  end

  defp verify_iss(%{fields: %{"iss" => iss}}, iss), do: :ok
  defp verify_iss(_, _), do: nil

  defp get_user_identity(token, fields, url) do
    token = "CF_Authorization=#{token}"
    fields = Enum.map(fields, &Atom.to_string/1)
    user = Req.request!(url: url, headers: [{"cookie", token}]).body
    if user, do: {:ok, Map.take(user, fields)}
  end

  defp identity(key) do
    %{
      key: key,
      key_type: "domain",
      iss: "https://#{key}.cloudflareaccess.com",
      certs: "https://#{key}.cloudflareaccess.com/cdn-cgi/access/certs",
      assertion: "cf-access-jwt-assertion",
      email: "cf-access-authenticated-user-email",
      identity: "https://#{key}.cloudflareaccess.com/cdn-cgi/access/get-identity"
    }
  end
end

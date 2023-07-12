defmodule Livebook.ZTA.GoogleIAP do
  @doc """
  To integrate your Google Identity-Aware Proxy (IAP) authentication with Livebook,
  set the `LIVEBOOK_IDENTITY_PROVIDER` environment variable to `google_iap:<your-jwt-audience>`

  For more information about Google IAP,
  see: https://cloud.google.com/iap/docs/concepts-overview

  Only access with Google accounts is supported
  See: https://cloud.google.com/iap/docs/authenticate-users-google-accounts

  For more details about how to find your JWT audience,
  see: https://cloud.google.com/iap/docs/signed-headers-howto
  and look for "Signed Header JWT Audience"
  """

  use GenServer
  require Logger
  import Plug.Conn

  @assertion "x-goog-iap-jwt-assertion"
  @renew_afer 24 * 60 * 60 * 1000
  @fields %{"sub" => :id, "name" => :name, "email" => :email}

  defstruct [:name, :req_options, :identity, :keys]

  def start_link(opts) do
    identity = opts[:custom_identity] || identity(opts[:identity][:key])
    options = [req_options: [url: identity.certs], identity: identity, keys: nil]
    GenServer.start_link(__MODULE__, options, name: opts[:name])
  end

  def authenticate(name, conn, fields: fields) do
    token = get_req_header(conn, @assertion)
    user = GenServer.call(name, {:authenticate, token, fields})
    {conn, user}
  end

  @impl true
  def init(options) do
    state = struct!(__MODULE__, options)
    {:ok, %{state | keys: keys(state)}}
  end

  @impl true
  def handle_call({:authenticate, token, fields}, _from, state) do
    user = authenticated_user(token, fields, state.identity, state.keys)
    {:reply, user, state}
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

  defp authenticated_user(token, _fields, identity, keys) do
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

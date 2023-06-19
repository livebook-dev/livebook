defmodule Livebook.ZTA.GoogleIAP do
  @moduledoc false

  use GenServer
  require Logger
  import Plug.Conn

  @name __MODULE__
  @renew_afer 24 * 60 * 60 * 1000

  defstruct [:name, :req_options]

  def start_link(_opts) do
    options = [name: @name, req_options: [url: identity().certs]]
    GenServer.start_link(__MODULE__, options, name: @name)
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

  def authenticate(conn, user_data) do
    with [token] <- get_req_header(conn, identity().assertion),
         {:ok, token} <- verify_token(token),
         :ok <- verify_iss(token) do
      Map.put(user_data, "name", token.fields["email"])
    else
      _ -> nil
    end
  end

  defp verify_token(token) do
    keys = GenServer.call(@name, :get_keys)

    Enum.find_value(keys, fn key ->
      case JOSE.JWT.verify(key, token) do
        {true, token, _s} -> {:ok, token}
        {_, _t, _s} -> nil
      end
    end)
  end

  defp verify_iss(%{fields: %{"iss" => iss}}), do: if(iss == identity().iss, do: :ok)

  defp identity() do
    {_, key} = Livebook.Config.identity_provider()

    %{
      key: key,
      key_type: "aud",
      iss: "https://cloud.google.com/iap",
      certs: "https://www.gstatic.com/iap/verify/public_key",
      assertion: "x-goog-iap-jwt-assertion",
      email: "x-goog-authenticated-user-email"
    }
  end
end

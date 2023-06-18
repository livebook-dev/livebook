defmodule Livebook.ZTA.Cloudflare do
  @moduledoc false

  use GenServer
  require Logger
  import Plug.Conn

  @name __MODULE__

  defstruct [:name, :req_options]

  def start_link(options) do
    options =
      options
      |> Keyword.put_new(:name, @name)
      |> Keyword.put_new(:req_options, url: identity().certs)

    GenServer.start_link(__MODULE__, options, name: options[:name])
  end

  def get() do
    get_from_ets(@name) || GenServer.call(@name, :get)
  end

  @impl true
  def init(options) do
    :ets.new(options[:name], [:public, :named_table])
    {:ok, struct!(__MODULE__, options)}
  end

  @impl true
  def handle_call(:get, _from, state) do
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
    Process.send_after(self(), :request, 24 * 60 * 60 * 1000)
    keys
  end

  defp get_from_ets(name) do
    case :ets.lookup(name, :keys) do
      [keys: keys] -> keys
      [] -> nil
    end
  rescue
    ArgumentError ->
      nil
  end

  def authenticate(conn) do
    with [token] <- get_req_header(conn, "cf-access-jwt-assertion"),
         {:ok, token} <- verify_token(token),
         :ok <- verify_iss(token) do
      true
    else
      _ -> false
    end
  end

  defp verify_token(token) do
    keys = get()

    Enum.find_value(keys, fn key ->
      case JOSE.JWT.verify(key, token) do
        {true, token, _s} -> {:ok, token}
        {_, _t, _s} -> nil
      end
    end)
  end

  defp verify_iss(%{fields: %{"iss" => iss}}), do: if iss == identity().iss, do: :ok

  def identity() do
    {provider, key} = Livebook.Config.identity_provider()

    %{
      provider: provider,
      key: key,
      key_type: "domain",
      iss: "https://#{key}.cloudflareaccess.com",
      certs: "https://#{key}.cloudflareaccess.com/cdn-cgi/access/certs",
      assertion: "cf-access-jwt-assertion"
    }
  end
end

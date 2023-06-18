defmodule Livebook.ZTIKeys do
  @moduledoc false

  # Periodically updates and stores the keys for a given identity provider

  use GenServer
  require Logger

  @name __MODULE__

  defstruct [:name, :req_options]

  def start_link(options) do
    identity = Livebook.ZTIIdentity.get()
    url = if identity, do: identity.certs

    options =
      options
      |> Keyword.put_new(:name, @name)
      |> Keyword.put_new(:req_options, url: url)

    GenServer.start_link(__MODULE__, options, name: options[:name])
  end

  def get(name \\ @name) do
    get_from_ets(name) || GenServer.call(name, :get)
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
end

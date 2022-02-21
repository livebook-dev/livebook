defmodule Livebook.Storage.Ets do
  @moduledoc """
  Ets implementation of `Livebook.Storage` behaviour.

  The module is supposed to be started just once as it
  is responsible for managing a named ets table.

  `insert` and `delete` operations are supposed to be called using a GenServer
  while all the lookups can be performed by directly accessing the named table.
  """
  @behaviour Livebook.Storage

  require Logger
  use GenServer

  @impl Livebook.Storage
  def all(namespace) do
    table_name()
    |> :ets.match({{namespace, :"$1"}, :"$2", :"$3", :_})
    |> Enum.group_by(
      fn [entity_id, _attr, _val] -> entity_id end,
      fn [_id, attr, val] -> {attr, val} end
    )
    |> Enum.map(fn {entity_id, attributes} ->
      attributes
      |> Map.new()
      |> Map.put(:id, entity_id)
    end)
  end

  @impl Livebook.Storage
  def fetch(namespace, entity_id) do
    table_name()
    |> :ets.lookup({namespace, entity_id})
    |> case do
      [] ->
        :error

      entries ->
        entries
        |> Enum.map(fn {_key, attr, val, _timestamp} -> {attr, val} end)
        |> Map.new()
        |> Map.put(:id, entity_id)
        |> then(&{:ok, &1})
    end
  end

  @impl Livebook.Storage
  def fetch_key(namespace, entity_id, key) do
    table_name()
    |> :ets.match({{namespace, entity_id}, key, :"$1", :_})
    |> case do
      [[value]] -> {:ok, value}
      [] -> :error
    end
  end

  @spec config_file_path() :: Path.t()
  def config_file_path() do
    Path.join([Livebook.Config.data_path(), "livebook_config.ets"])
  end

  @spec sync() :: :ok
  def sync() do
    GenServer.call(__MODULE__, :sync)
  end

  @impl Livebook.Storage
  def insert(namespace, entity_id, attributes) do
    GenServer.call(__MODULE__, {:insert, namespace, entity_id, attributes})
  end

  @impl Livebook.Storage
  def delete(namespace, entity_id) do
    GenServer.call(__MODULE__, {:delete, namespace, entity_id})
  end

  @impl Livebook.Storage
  def delete_key(namespace, entity_id, key) do
    GenServer.call(__MODULE__, {:delete_key, namespace, entity_id, key})
  end

  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl GenServer
  def init(_opts) do
    # Make sure that this process does not terminate abruptly
    # in case it is persisting to disk. terminate/2 is still a no-op.
    Process.flag(:trap_exit, true)

    table = load_or_create_table()
    :persistent_term.put(__MODULE__, table)

    {:ok, %{table: table}}
  end

  @impl GenServer
  def handle_call(:sync, _from, state) do
    {:reply, :ok, state}
  end

  @impl GenServer
  def handle_call({:insert, namespace, entity_id, attributes}, _from, %{table: table} = state) do
    keys_to_delete = Enum.map(attributes, fn {key, _val} -> key end)

    delete_keys(table, namespace, entity_id, keys_to_delete)

    timestamp = System.os_time(:millisecond)

    attributes =
      Enum.map(attributes, fn {attr, val} ->
        {{namespace, entity_id}, attr, val, timestamp}
      end)

    :ets.insert(table, attributes)
    {:reply, :ok, state, {:continue, :save_to_file}}
  end

  @impl GenServer
  def handle_call({:delete, namespace, entity_id}, _from, %{table: table} = state) do
    :ets.delete(table, {namespace, entity_id})
    {:reply, :ok, state, {:continue, :save_to_file}}
  end

  @impl GenServer
  def handle_call({:delete_key, namespace, entity_id, key}, _from, %{table: table} = state) do
    delete_keys(table, namespace, entity_id, [key])
    {:reply, :ok, state, {:continue, :save_to_file}}
  end

  @impl GenServer
  def handle_continue(:save_to_file, %{table: table} = state) do
    file_path = String.to_charlist(config_file_path())
    :ok = :ets.tab2file(table, file_path)
    {:noreply, state}
  end

  defp table_name(), do: :persistent_term.get(__MODULE__)

  defp load_or_create_table() do
    config_file_path()
    |> String.to_charlist()
    |> :ets.file2tab()
    |> case do
      {:ok, tab} ->
        tab

      {:error, reason} ->
        case reason do
          {:read_error, {:file_error, _, :enoent}} -> :ok
          _ -> Logger.warning("Could not open up #{config_file_path()}: #{inspect(reason)}")
        end

        :ets.new(__MODULE__, [:protected, :duplicate_bag])
    end
  end

  defp delete_keys(table, namespace, entity_id, keys) do
    match_head = {{namespace, entity_id}, :"$1", :_, :_}

    guards = Enum.map(keys, &{:==, :"$1", &1})

    :ets.select_delete(table, [{match_head, guards, [true]}])
  end
end

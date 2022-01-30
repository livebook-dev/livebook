defmodule Livebook.Storage.Ets do
  @moduledoc """
  Ets implementation of `Livebook.Storage` behaviour.

  The module is supposed to be started just once as it
  is responsible for managing a single global named ets table.

  `insert` and `delete` operations are supposed to be called using the genserver
  while all the lookups can be performed by directly accessing the named table.
  """
  @behaviour Livebook.Storage

  @table_name :__livebook_storage__

  use GenServer

  @impl Livebook.Storage
  def fetch(namespace, entity_id) do
    @table_name
    |> :ets.lookup({namespace, entity_id})
    |> case do
      [] ->
        :error

      entries ->
        entries
        |> Enum.map(fn {_key, attr, val, _timestamp} ->
          {attr, val}
        end)
        |> Map.new()
        |> Map.put(:id, entity_id)
        |> then(&{:ok, &1})
    end
  end

  @impl Livebook.Storage
  def all(namespace) do
    @table_name
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
  def insert(namespace, entity_id, attributes) do
    GenServer.call(__MODULE__, {:insert, namespace, entity_id, attributes})
  end

  @impl Livebook.Storage
  def delete(namespace, entity_id) do
    GenServer.call(__MODULE__, {:delete, namespace, entity_id})
  end

  @spec start_link() :: GenServer.on_start()
  def start_link() do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl GenServer
  def init(_opts) do
    table = :ets.new(@table_name, [:named_table, :protected, :duplicate_bag])

    {:ok, %{table: table}}
  end

  @impl GenServer
  def handle_call({:insert, namespace, entity_id, attributes}, _from, %{table: table} = state) do
    new_attributes =
      attributes
      |> Map.new(fn {key, value} ->
        {key, {value, System.os_time(:millisecond)}}
      end)

    old_attributes =
      table
      |> :ets.lookup({{namespace, entity_id}, :"$1", :"$2", :"$3"})
      |> Map.new(fn [key, value, timestamp] ->
        {key, {value, timestamp}}
      end)

    # delete the whole entity to insert all its entries once again
    # it is due to a limitation of duplicate bag where a single value can't be deleted nor updated
    # therefore we are basically reinserting all attributes
    :ets.delete(table, {namespace, entity_id})

    old_attributes
    |> Map.merge(new_attributes)
    |> Enum.map(fn {key, {value, timestamp}} ->
      :ets.insert(table, {{namespace, entity_id}, key, value, timestamp})
    end)

    {:ok, entity} = fetch(namespace, entity_id)

    {:reply, entity, state}
  end

  @impl GenServer
  def handle_call({:delete, namespace, entity_id}, _from, %{table: table} = state) do
    :ets.delete(table, {namespace, entity_id})

    {:reply, :ok, state}
  end
end

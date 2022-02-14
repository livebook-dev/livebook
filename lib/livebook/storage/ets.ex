defmodule Livebook.Storage.Ets do
  @moduledoc """
  Ets implementation of `Livebook.Storage` behaviour.

  The module is supposed to be started just once as it
  is responsible for managing a named ets table.

  `insert` and `delete` operations are supposed to be called using a GenServer
  while all the lookups can be performed by directly accessing the named table.
  """
  @behaviour Livebook.Storage

  @table_name __MODULE__

  @persistence_timeout 5_000

  @sign_secret "livebook-ets"

  use GenServer

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
  def fetch(namespace, entity_id) do
    @table_name
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
    @table_name
    |> :ets.match({{namespace, entity_id}, key, :"$1", :_})
    |> case do
      [[value]] -> {:ok, value}
      [] -> :error
    end
  end

  @impl Livebook.Storage
  def insert(namespace, entity_id, attributes) do
    GenServer.call(__MODULE__, {:insert, namespace, entity_id, attributes})
  end

  @impl Livebook.Storage
  def delete(namespace, entity_id) do
    GenServer.call(__MODULE__, {:delete, namespace, entity_id})
  end

  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl GenServer
  def init(opts) do
    # enable passing table name for testing purposes
    table_name = Keyword.get(opts, :table_name, @table_name)
    table = :ets.new(table_name, [:named_table, :protected, :duplicate_bag])

    # NOTE: should we always start persisting to file or make user state that explicitly?
    secret = Keyword.get(opts, :secret, default_secret())
    file_path = Keyword.get(opts, :file_path, default_file_path())

    state = %{table: table, pending_persist: nil, secret: secret, file_path: file_path}

    {:ok, load_from_file(state)}
  end

  @impl GenServer
  def handle_call({:insert, namespace, entity_id, attributes}, _from, %{table: table} = state) do
    match_head = {{namespace, entity_id}, :"$1", :_, :_}

    guards =
      Enum.map(attributes, fn {key, _val} ->
        {:==, :"$1", key}
      end)

    :ets.select_delete(table, [{match_head, guards, [true]}])

    timestamp = System.os_time(:millisecond)

    attributes =
      Enum.map(attributes, fn {attr, val} ->
        {{namespace, entity_id}, attr, val, timestamp}
      end)

    :ets.insert(table, attributes)

    {:reply, :ok, schedule_persist(state)}
  end

  @impl GenServer
  def handle_call({:delete, namespace, entity_id}, _from, %{table: table} = state) do
    :ets.delete(table, {namespace, entity_id})

    {:reply, :ok, schedule_persist(state)}
  end

  @impl GenServer
  def handle_info(:persist, state) do
    state = %{state | pending_persist: nil}

    {:noreply, save_to_file(state)}
  end

  defp schedule_persist(%{pending_persist: ref} = state) do
    unless is_nil(ref) do
      Process.cancel_timer(ref)
    end

    ref = Process.send_after(self(), :persist, @persistence_timeout)

    %{state | pending_persist: ref}
  end

  defp default_secret() do
    Livebook.Config.secret!("LIVEBOOK_SECRET_KEY_BASE")
  end

  defp default_file_path() do
    # NOTE: should we add the application port to distinguish between multiple instances?
    Path.join([:filename.basedir(:user_cache, "livebook"), "livebook.conf"])
  end

  defp load_from_file(%{file_path: file_path, secret: secret} = state)
       when is_nil(file_path) or is_nil(secret) do
    state
  end

  defp load_from_file(%{table: table, file_path: file_path, secret: secret} = state) do
    # NOTE: should we silently ignore a case where the file does not exist yet?
    if File.exists?(file_path) do
      file_path
      |> load()
      |> decrypt(secret)
      |> :erlang.binary_to_term()
      |> then(&:ets.insert(table, &1))
    end

    state
  end

  defp save_to_file(%{file_path: file_path, secret: secret} = state)
       when is_nil(file_path) or is_nil(secret) do
    state
  end

  defp save_to_file(%{table: table, file_path: file_path, secret: secret} = state) do
    table
    |> :ets.tab2list()
    |> :erlang.term_to_binary()
    |> encrypt(secret)
    |> persist(file_path)

    state
  end

  defp persist(content, file_path) do
    File.write!(file_path, content)
  end

  defp encrypt(payload, secret) do
    Plug.Crypto.MessageEncryptor.encrypt(payload, secret, @sign_secret)
  end

  defp load(file_path) do
    File.read!(file_path)
  end

  defp decrypt(payload, secret) do
    {:ok, content} = Plug.Crypto.MessageEncryptor.decrypt(payload, secret, @sign_secret)

    content
  end
end

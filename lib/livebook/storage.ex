defmodule Livebook.Storage do
  # Storage for arbitrary data in [Entity-Attribute-Value](https://en.wikipedia.org/wiki/Entity%E2%80%93attribute%E2%80%93value_model)
  # fashion.
  #
  # The storage uses an ETS table and synchronizes all the changes to
  # a file, so they are restored when the application is started again.
  #
  # `insert` and `delete` operations are supposed to be called using a GenServer
  # while all the lookups can be performed by directly accessing the named table.

  use GenServer

  require Logger

  @type namespace :: atom()
  @type entity_id :: binary()
  @type attribute :: atom()
  @type value :: term()
  @type timestamp :: non_neg_integer()
  @type entity :: %{required(:id) => entity_id(), optional(attribute()) => value()}

  defmodule NotFoundError do
    defexception [:id, :namespace, plug_status: 404]

    def message(%{namespace: namespace, id: id}) do
      "could not find entry in \"#{namespace}\" with ID #{inspect(id)}"
    end
  end

  @doc """
  Starts the storage process.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Returns all values in namespace.

  ## Examples

      Livebook.Storage.all(:filesystem)
      #=> [%{id: "rand-id", type: "s3", bucket_url: "/...", secret: "abc", access_key: "xyz"}]

  """
  @spec all(namespace()) :: [entity()]
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

  @doc """
  Returns a map identified by `entity_id` in `namespace`.

  ## Examples

      Livebook.Storage.fetch(:filesystem, "rand-id")
      #=> {:ok, %{id: "rand-id", type: "s3", bucket_url: "/...", secret: "abc", access_key: "xyz"}}

  """
  @spec fetch(namespace(), entity_id()) :: {:ok, entity()} | :error
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

  @doc """
  Raising delegate for `c:fetch/2`.
  """
  @spec fetch!(namespace(), entity_id()) :: entity()
  def fetch!(namespace, id) do
    case fetch(namespace, id) do
      {:ok, entity} -> entity
      :error -> raise NotFoundError, namespace: namespace, id: id
    end
  end

  @doc """
  Returns the value for a given `namespace`-`entity_id`-`attribute`.

  ## Examples

      Livebook.Storage.fetch_key(:filesystem, "rand-id", :type)
      #=> {:ok, "s3"}

  """
  @spec fetch_key(namespace(), entity_id(), attribute()) :: {:ok, value()} | :error
  def fetch_key(namespace, entity_id, key) do
    table_name()
    |> :ets.match({{namespace, entity_id}, key, :"$1", :_})
    |> case do
      [[value]] -> {:ok, value}
      [] -> :error
    end
  end

  @doc """
  Inserts given list of attribute-value pairs to a entity belonging to
  specified namespace.
  """
  @spec insert(namespace(), entity_id(), [{attribute(), value()}]) :: :ok
  def insert(namespace, entity_id, attributes) do
    GenServer.call(__MODULE__, {:insert, namespace, entity_id, attributes})
  end

  @doc """
  Deletes an entity of given id from given namespace.
  """
  @spec delete(namespace(), entity_id()) :: :ok
  def delete(namespace, entity_id) do
    GenServer.call(__MODULE__, {:delete, namespace, entity_id})
  end

  @doc """
  Deletes an attribute from given entity.
  """
  @spec delete_key(namespace(), entity_id(), attribute()) :: :ok
  def delete_key(namespace, entity_id, key) do
    GenServer.call(__MODULE__, {:delete_key, namespace, entity_id, key})
  end

  defp config_file_path() do
    migration_version = Livebook.Migration.migration_version()
    Path.join([Livebook.Config.data_path(), "livebook_config.v#{migration_version}.ets"])
  end

  defp config_file_path_for_restore() do
    # We look for a file matching the current expected version, or an
    # older one that will get migrated.
    #
    # There may be files with a more recent version in case the user
    # downgrated Livebook, and those files we ignore.

    migration_version = Livebook.Migration.migration_version()
    dir = Livebook.Config.data_path()

    names =
      case File.ls(dir) do
        {:ok, names} -> names
        {:error, _} -> []
      end

    candidates =
      for name <- names, version = file_version(name), version <= migration_version do
        %{name: name, version: version}
      end

    if candidates != [] do
      %{name: name} = Enum.max_by(candidates, & &1.version)
      Path.join([dir, name])
    end
  end

  defp file_version(name) do
    case String.split(name, ".") do
      ["livebook_config", "ets"] -> 0
      ["livebook_config", "v" <> version, "ets"] -> String.to_integer(version)
      _ -> nil
    end
  end

  @impl true
  def init(_opts) do
    # Make sure that this process does not terminate abruptly
    # in case it is persisting to disk. terminate/2 is still a no-op.
    Process.flag(:trap_exit, true)

    persist_storage? = Application.get_env(:livebook, :persist_storage, true)
    table = load_or_create_table(persist_storage?)
    :persistent_term.put(__MODULE__, table)

    {:ok, %{table: table, persist?: persist_storage?}}
  end

  @impl true
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

  def handle_call({:delete, namespace, entity_id}, _from, %{table: table} = state) do
    :ets.delete(table, {namespace, entity_id})
    {:reply, :ok, state, {:continue, :save_to_file}}
  end

  def handle_call({:delete_key, namespace, entity_id, key}, _from, %{table: table} = state) do
    delete_keys(table, namespace, entity_id, [key])
    {:reply, :ok, state, {:continue, :save_to_file}}
  end

  @impl true
  def handle_continue(:save_to_file, %{persist?: false} = state) do
    {:noreply, state}
  end

  def handle_continue(:save_to_file, %{table: table} = state) do
    file_path = String.to_charlist(config_file_path())
    :ok = :ets.tab2file(table, file_path)
    {:noreply, state}
  end

  defp table_name(), do: :persistent_term.get(__MODULE__)

  defp load_or_create_table(false) do
    :ets.new(__MODULE__, [:protected, :duplicate_bag])
  end

  defp load_or_create_table(true) do
    tab =
      if path = config_file_path_for_restore() do
        path
        |> String.to_charlist()
        |> :ets.file2tab()
        |> case do
          {:ok, tab} ->
            Logger.info("Reading storage from #{path}")
            tab

          {:error, reason} ->
            Logger.warning("Could not open up #{path}: #{inspect(reason)}")
            nil
        end
      end

    tab || :ets.new(__MODULE__, [:protected, :duplicate_bag])
  end

  defp delete_keys(table, namespace, entity_id, keys) do
    match_head = {{namespace, entity_id}, :"$1", :_, :_}

    guard =
      keys
      |> Enum.map(&{:==, :"$1", &1})
      |> Enum.reduce(&{:orelse, &1, &2})

    :ets.select_delete(table, [{match_head, [guard], [true]}])
  end
end

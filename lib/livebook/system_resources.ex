defmodule Livebook.SystemResources do
  @moduledoc false

  # Periodically computes system resource usage.

  @type memory :: %{total: non_neg_integer(), free: non_neg_integer()}

  use GenServer
  @name __MODULE__

  @doc """
  Returns system memory.
  """
  @spec memory() :: memory()
  def memory do
    :ets.lookup_element(@name, :memory, 2)
  end

  @doc """
  Subscribes to resource usage updates.

  ## Messages

    * `{:memory_update, memory}`

  """
  @spec subscribe() :: :ok | {:error, term()}
  def subscribe() do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "system_resources")
  end

  @doc """
  Updates the resources kept by this process.
  """
  @spec update() :: :ok
  def update do
    GenServer.cast(@name, :update)
  end

  @doc false
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  @impl true
  def init(:ok) do
    :ets.new(@name, [:set, :named_table, :protected])
    measure()
    schedule()
    {:ok, %{}}
  end

  @impl true
  def handle_info(:measure, state) do
    measure()
    schedule()
    {:noreply, state}
  end

  @impl true
  def handle_cast(:update, state) do
    memory = measure()
    Phoenix.PubSub.local_broadcast(Livebook.PubSub, "system_resources", {:memory_update, memory})
    {:noreply, state}
  end

  defp measure() do
    memory_data = :memsup.get_system_memory_data()
    memory = %{total: memory_data[:total_memory], free: memory_data[:free_memory]}
    :ets.insert(@name, {:memory, memory})
    memory
  end

  defp schedule() do
    Process.send_after(self(), :measure, 15000)
  end
end

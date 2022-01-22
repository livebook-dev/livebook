defmodule Livebook.SystemResources do
  # Periodically compute system resource usage.
  @moduledoc false
  @type memory :: %{total: non_neg_integer(), free: non_neg_integer()}

  use GenServer
  @name __MODULE__

  @spec memory() :: memory()
  def memory do
    :ets.lookup_element(@name, :memory, 2)
  end

  @doc false
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  @impl true
  def init(:ok) do
    :ets.new(@name, [:set, :named_table, :protected])
    measure()
    {:ok, %{}}
  end

  @impl true
  def handle_info(:measure, state) do
    measure()
    schedule_measurement()
    {:noreply, state}
  end

  defp schedule_measurement() do
    Process.send_after(self(), :measure, 15000)
  end

  defp measure() do
    memory = :memsup.get_system_memory_data()
    :ets.insert(@name, {:memory, %{total: memory[:total_memory], free: memory[:free_memory]}})
  end
end

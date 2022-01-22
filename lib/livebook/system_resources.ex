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
    measure_and_schedule()
    {:ok, %{}}
  end

  @impl true
  def handle_info(:measure, state) do
    measure_and_schedule()
    {:noreply, state}
  end

  defp measure_and_schedule() do
    memory = :memsup.get_system_memory_data()
    :ets.insert(@name, {:memory, %{total: memory[:total_memory], free: memory[:free_memory]}})
    Process.send_after(self(), :measure, 15000)
  end
end

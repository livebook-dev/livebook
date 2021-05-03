defmodule Livebook.Runtime.NodePool do
  use GenServer, async: false

  # A pool for reusing child node names.
  #
  # `pool` refers to the list of unused names
  #
  # `buffer_time` refers to time the pool waits before
  # adding a name to `pool`,  which by default is 1 minute.

  @default_time 6_000

  # Client interface

  @doc """
  Starts the node pool with the buffer time as 1 minute.
  """
  def start do
    GenServer.start(__MODULE__, @default_time, name: __MODULE__)
  end

  @doc """
  Starts the node pool with the buffer time as input.
  """
  def start(buffer_time) do
    GenServer.start(__MODULE__, buffer_time, name: __MODULE__)
  end

  @doc """
  Retuns a node name.

  Generates a new one if node is empty, or takes on from pool.
  """
  def get_name(parent) do
    GenServer.call(__MODULE__, {:get_name, parent})
  end

  # Server side code

  @impl GenServer
  def init(buffer_time) do
    :net_kernel.monitor_nodes(true)
    {:ok, %{pool: [], buffer_time: buffer_time}}
  end

  @impl GenServer
  def handle_call({:get_name, parent}, _, state) do
    {:reply, name(state, parent), state}
  end

  @impl GenServer
  def handle_info({:nodeup, node}, state) do
    {:noreply, remove_node(state, node)}
  end

  @impl GenServer
  def handle_info({:nodedown, node}, state) do
    {:ok, _TRef} = :timer.send_after(state.buffer_time, __MODULE__, {:add_node, node})
    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:add_node, node}, state) do
    {:noreply, add_node(state, node)}
  end

  # Helper functions

  defp name(state, parent) do
    if Enum.empty?(state.pool) do
      :"#{Livebook.Utils.random_short_id()}-#{parent}"
    else
      hd(state.pool)
    end
  end

  defp remove_node(state, node) do
    %{state | pool: List.delete(state.pool, node)}
  end

  defp add_node(state, node) do
    %{state | pool: [node | state.pool]}
  end
end

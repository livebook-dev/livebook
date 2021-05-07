defmodule Livebook.Runtime.NodePool do
  use GenServer

  @moduledoc false

  # A pool for reusing child node names.
  #
  # `free_name` refers to the list of unused names.
  # `generated_names` refers to the list of names ever generated.
  #
  # `buffer_time` refers to time the pool waits before
  # adding a name to `pool`,  which by default is 1 minute.

  @default_time 60_000

  # Client interface

  @doc """
  Starts the GenServer from a Supervision tree

  ## Options

    - `:name` - The name the NodePool is locally registered as. By default, it is `Livebook.Runtime.NodePool`
    - `:buffer_time` - The time that is spent before a disconnected node's name is  added to pool. The default is 1 minute.
  """
  def start_link(opts) do
    name = opts[:name] || __MODULE__
    buffer_time = opts[:buffer_time] || @default_time

    GenServer.start_link(
      __MODULE__,
      %{buffer_time: buffer_time},
      name: name
    )
  end

  @doc """
  Retuns a node name.

  Generates a new name if pool is empty, or takes one from pool.
  """
  def get_name(server \\ __MODULE__, basename) do
    GenServer.call(server, {:get_name, basename})
  end

  # Server side code

  @impl GenServer
  def init(opts) do
    :net_kernel.monitor_nodes(true, node_type: :all)
    {:ok, %{buffer_time: opts.buffer_time, generated_names: MapSet.new(), free_names: []}}
  end

  @impl GenServer
  def handle_call({:get_name, basename}, _, state) do
    {name, new_state} = name(state, basename)
    {:reply, name, new_state}
  end

  @impl GenServer
  def handle_info({:nodedown, node, _info}, state) do
    _ = Process.send_after(self(), {:add_node, node}, state.buffer_time)
    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:nodeup, _node, _info}, state) do
    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:add_node, node}, state) do
    {:noreply, add_node(state, node)}
  end

  # Helper functions

  defp name(state, basename) do
    if Enum.empty?(state.free_names) do
      generate_name(state, basename)
    else
      get_existing_name(state)
    end
  end

  defp generate_name(state, basename) do
    new_name = :"#{Livebook.Utils.random_short_id()}-#{basename}"
    generated_names = MapSet.put(state.generated_names, new_name)
    {new_name, %{state | generated_names: generated_names}}
  end

  defp get_existing_name(state) do
    [name | free_names] = state.free_names
    {name, %{state | free_names: free_names}}
  end

  defp add_node(state, node) do
    if MapSet.member?(state.generated_names, node) do
      free_names = [node | state.free_names]
      %{state | free_names: free_names}
    else
      state
    end
  end
end

defmodule Livebook.Runtime.NodePool do
  use GenServer

  @moduledoc false

  # A pool for reusing child node names.
  #
  # `own_pool` refers to the list of unused names.
  # `given_pool` refers to the list of names ever generated.
  #
  # `buffer_time` refers to time the pool waits before
  # adding a name to `pool`,  which by default is 1 minute.

  @default_time 60_000

  # Client interface

  @doc """
  Starts the GenServer from a Supervision tree
  """
  def start_link(opts) do
    # Note that the following options are for testing only.
    name = opts[:name] || __MODULE__
    buffer_time = opts[:buffer_time] || @default_time
    given_pool = opts[:given_pool] || []
    own_pool = opts[:own_pool] || []

    GenServer.start_link(
      __MODULE__,
      %{buffer_time: buffer_time, given_pool: given_pool, own_pool: own_pool},
      name: name
    )
  end

  @doc """
  Retuns a node name.

  Generates a new name if pool is empty, or takes one from pool.
  """
  def get_name(basename, server \\ __MODULE__) do
    GenServer.call(server, {:get_name, basename})
  end

  # Server side code

  @impl GenServer
  def init(args) do
    :net_kernel.monitor_nodes(true, [{:node_type, :all}])
    {:ok, args}
  end

  @impl GenServer
  def handle_call({:get_name, basename}, _, state) do
    {name, new_state} = name(state, basename)
    {:reply, name, new_state}
  end

  @impl GenServer
  def handle_info({:nodedown, node, _info}, state) do
    {:ok, _} = :timer.send_after(state.buffer_time, __MODULE__, {:add_node, node})
    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:nodeup, _node, _info}, state) do
    # We don't want our mailbox full of node up messages
    {:noreply, state}
  end

  @impl GenServer
  def handle_info({:add_node, node}, state) do
    {:noreply, add_node(state, node)}
  end

  # Helper functions

  defp name(state, basename) do
    if Enum.empty?(state.own_pool) do
      generate_name(state, basename)
    else
      get_existing_name(state)
    end
  end

  defp generate_name(state, basename) do
    new_name = :"#{Livebook.Utils.random_short_id()}-#{basename}"
    {new_name, %{state | given_pool: [new_name | state.given_pool]}}
  end

  defp get_existing_name(state) do
    {name, own_pool} = List.pop_at(state.own_pool, 0)
    {name, %{state | own_pool: own_pool}}
  end

  defp add_node(state, node) do
    if Enum.member?(state.given_pool, node) do
      %{state | own_pool: [node | state.own_pool]}
    else
      state
    end
  end
end

defmodule Livebook.EPMD.NodePool do
  use GenServer

  # A pool with generated node names.
  #
  # The names are randomly generated, however to avoid atom exhaustion
  # unused names return back to the pool and can be reused later.

  @default_time 60_000

  # Client interface

  @doc """
  Starts the GenServer from a Supervision tree

  ## Options

    * `:name` - the name to register the pool process under. Defaults
      to `Livebook.Runtime.NodePool`

    * `:buffer_time` - the time that is awaited before a disconnected
      node's name is added to pool. Defaults to 1 minute

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
  Returns a node name.

  Generates a new name if pool is empty, or takes one from pool.
  """
  def get_name(server \\ __MODULE__) do
    GenServer.call(server, :get_name, :infinity)
  end

  @doc """
  Returns port for the given name.
  """
  def get_port(server \\ __MODULE__, name) do
    GenServer.call(server, {:get_port, name}, :infinity)
  end

  @doc """
  Updates a port for a name.
  """
  def update_name(server \\ __MODULE__, name, port) do
    GenServer.call(server, {:update_name, name, port}, :infinity)
  end

  # Server side code

  @impl true
  def init(opts) do
    :net_kernel.monitor_nodes(true, node_type: :all)
    [name, host] = node() |> Atom.to_string() |> :binary.split("@")

    state = %{
      buffer_time: opts.buffer_time,
      active_names: %{},
      free_names: [],
      prefix: name,
      host: host
    }

    {:ok, state}
  end

  @impl true
  def handle_call(:get_name, _, state) do
    {name, state} = server_get_name(state)
    {:reply, name, put_in(state.active_names[name], 0)}
  end

  def handle_call({:get_port, name}, _, state) do
    {:reply, Map.get(state.active_names, name, 0), state}
  end

  def handle_call({:update_name, name, port}, _, state) do
    {:reply, :ok, server_update_name(name, port, state)}
  end

  @impl true
  def handle_info({:nodedown, node, _info}, state) do
    case state.buffer_time do
      0 -> send(self(), {:release_node, node})
      t -> Process.send_after(self(), {:release_node, node}, t)
    end

    {:noreply, state}
  end

  def handle_info({:nodeup, _node, _info}, state) do
    {:noreply, state}
  end

  def handle_info({:release_node, node}, state) do
    {:noreply, server_release_name(Atom.to_string(node), state)}
  end

  # Helper functions

  defp server_get_name(state) do
    case state.free_names do
      [] -> {server_generate_name(state), state}
      [name | free_names] -> {name, %{state | free_names: free_names}}
    end
  end

  defp server_update_name(name, port, state) do
    case state.active_names do
      %{^name => _} -> put_in(state.active_names[name], port)
      %{} -> state
    end
  end

  defp server_generate_name(%{prefix: prefix, host: host}) do
    "#{prefix}--#{Livebook.Utils.random_short_id()}@#{host}"
  end

  defp server_release_name(name, state) do
    {port, state} = pop_in(state.active_names[name])

    if port do
      %{state | free_names: [name | state.free_names]}
    else
      state
    end
  end
end

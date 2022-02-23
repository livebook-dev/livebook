defmodule Livebook.Runtime.ErlDist.IOForwardGL do
  @moduledoc false

  # An IO device process forwarding all requests to sender's group
  # leader.
  #
  # We register this device as the `:standard_error` in the runtime
  # node, so that all evaluation warnings are treated as stdout.
  #
  # The process implements [The Erlang I/O Protocol](https://erlang.org/doc/apps/stdlib/io_protocol.html)
  # and can be thought of as a virtual IO device.

  use GenServer

  @doc """
  Starts the IO device.

  ## Options

    * `:name` - the name to register the process under. Optional.
      If the name is already used, it will be unregistered before
      starting the process and registered back when the server
      terminates.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    name = opts[:name]

    if previous = name && Process.whereis(name) do
      Process.unregister(name)
    end

    GenServer.start_link(__MODULE__, {name, previous}, opts)
  end

  @impl true
  def init({name, previous}) do
    Process.flag(:trap_exit, true)
    {:ok, %{name: name, previous: previous}}
  end

  @impl true
  def handle_info({:io_request, from, reply_as, req}, state) do
    case Process.info(from, :group_leader) do
      {:group_leader, group_leader} ->
        # Forward the request to sender's group leader
        # and instruct it to get back to us.
        send(group_leader, {:io_request, from, reply_as, req})

      _ ->
        send(from, {:io_reply, reply_as, {:error, :terminated}})
    end

    {:noreply, state}
  end

  @impl true
  def terminate(_, %{name: name, previous: previous}) do
    if name && previous do
      Process.unregister(name)
      Process.register(previous, name)
    end

    :ok
  end
end

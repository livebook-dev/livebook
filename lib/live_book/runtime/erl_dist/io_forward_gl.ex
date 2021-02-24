defmodule LiveBook.Runtime.ErlDist.IOForwardGL do
  @moduledoc false

  # An IO device process forwarding all requests to sender's group leader.
  #
  # We use this device as `:standard_error` on connected runtime node,
  # so that all evaluation warnings are treated as stdout.
  #
  # The process implements [The Erlang I/O Protocol](https://erlang.org/doc/apps/stdlib/io_protocol.html)
  # and can be thought of as a *virtual* IO device.

  use GenServer

  ## API

  @spec start_link() :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  ## Callbacks

  @impl true
  def init(_opts) do
    {:ok, %{requests: %{}}}
  end

  @impl true
  def handle_info({:io_request, from, reply_as, req}, state) do
    # Forward the request to sender's group leader
    # and instruct it to get back to us.
    {:group_leader, group_leader} = Process.info(from, :group_leader)
    send(group_leader, {:io_request, self(), reply_as, req})

    state = put_in(state.requests[reply_as], from)

    {:noreply, state}
  end

  def handle_info({:io_reply, reply_as, reply}, state) do
    # Send forward the reply from group leader to the original client.
    {initially_from, state} = pop_in(state.requests[reply_as])
    send(initially_from, {:io_reply, reply_as, reply})

    {:noreply, state}
  end
end

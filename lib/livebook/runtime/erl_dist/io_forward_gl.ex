defmodule Livebook.Runtime.ErlDist.IOForwardGL do
  @moduledoc false

  # An IO device process forwarding all requests to sender's group leader.
  #
  # We use this device as `:standard_error` on connected runtime node,
  # so that all evaluation warnings are treated as stdout.
  #
  # The process implements [The Erlang I/O Protocol](https://erlang.org/doc/apps/stdlib/io_protocol.html)
  # and can be thought of as a *virtual* IO device.

  use GenServer

  @type state :: %{(reply_as :: term()) => from :: pid()}

  ## API

  @doc """
  Starts the IO device.

  ## Options

    * `:name` - the name to register the process under. Optional.
      If the name is already used, it will be unregistered before
      starting the process and registered back when the server
      terminates.
  """
  @spec start_link() :: GenServer.on_start()
  def start_link(opts \\ []) do
    name = opts[:name]

    if previous = name && Process.whereis(name) do
      Process.unregister(name)
    end

    GenServer.start_link(__MODULE__, {name, previous}, opts)
  end

  ## Callbacks

  @impl true
  def init({name, previous}) do
    Process.flag(:trap_exit, true)
    {:ok, %{previous: {name, previous}, replies: %{}}}
  end

  @impl true
  def handle_info({:io_request, from, reply_as, req}, state) do
    case Process.info(from, :group_leader) do
      {:group_leader, group_leader} ->
        # Forward the request to sender's group leader
        # and instruct it to get back to us.
        send(group_leader, {:io_request, self(), reply_as, req})
        state = put_in(state.replies[reply_as], from)

        {:noreply, state}

      _ ->
        {:noreply, state}
    end
  end

  def handle_info({:io_reply, reply_as, reply}, state) do
    # Forward the reply from group leader to the original client.
    {initially_from, state} = pop_in(state.replies[reply_as])
    send(initially_from, {:io_reply, reply_as, reply})

    {:noreply, state}
  end

  @impl true
  def terminate(_, %{previous: {name, previous}}) do
    if name && previous do
      Process.unregister(name)
      Process.register(previous, name)
    end

    :ok
  end
end

defmodule ElixirKit.Server do
  @moduledoc false

  use GenServer

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(pid) do
    port = System.fetch_env!("ELIXIRKIT_PORT") |> String.to_integer()
    {:ok, socket} = :gen_tcp.connect('localhost', port, mode: :binary, packet: 4)
    {:ok, %{pid: pid, socket: socket}}
  end

  @impl true
  def handle_call({:send_event, name, data}, _from, state) do
    payload = [name, ?:, data]
    :ok = :gen_tcp.send(state.socket, payload)
    {:reply, :ok, state}
  end

  @impl true
  def handle_info({:tcp, socket, payload}, state) when socket == state.socket do
    [name, data] = :binary.split(payload, ":")
    send(state.pid, {:event, name, data})
    {:noreply, state}
  end

  @impl true
  def handle_info({:tcp_closed, socket}, state) when socket == state.socket do
    {:stop, :shutdown, state}
  end
end

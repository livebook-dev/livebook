defmodule ElixirKit.Server do
  @moduledoc false

  use GenServer

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(pid) do
    port = System.fetch_env!("ELIXIRKIT_PORT") |> String.to_integer()
    {:ok, socket} = :gen_tcp.connect('localhost', port, mode: :binary, packet: :line)
    {:ok, %{pid: pid, socket: socket}}
  end

  @impl true
  def handle_info({:tcp, socket, "event:" <> rest}, state) when socket == state.socket do
    [name, data] = rest |> String.trim_trailing() |> String.split(":")
    data = Base.decode64!(data)
    send(state.pid, {:event, name, data})
    {:noreply, state}
  end

  @impl true
  def handle_info({:tcp_closed, socket}, state) when socket == state.socket do
    System.stop()
    {:stop, :shutdown, state}
  end
end

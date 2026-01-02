defmodule Example.Server do
  @moduledoc false

  use GenServer

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_) do
    {:ok, pid} = ElixirKit.start()
    ref = Process.monitor(pid)

    ElixirKit.publish("ready", "ready")

    {:ok, %{ref: ref}}
  end

  @impl true
  def handle_info({:event, "ping", "ping"}, state) do
    IO.puts("[elixir] ping")
    ElixirKit.publish("echo", "pong")
    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _, :shutdown}, state) when ref == state.ref do
    System.stop()
    {:stop, :shutdown, state}
  end

end

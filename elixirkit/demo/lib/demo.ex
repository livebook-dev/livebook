defmodule Demo.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      Demo.Server
    ]

    opts = [strategy: :one_for_one, name: Demo.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

defmodule Demo.Server do
  use GenServer

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_) do
    Process.flag(:trap_exit, true)

    {:ok, pid} = ElixirKit.start()
    ref = Process.monitor(pid)

    ElixirKit.publish("log", "Hello from Elixir!")

    Task.start(fn ->
      for i <- 5..1//-1 do
        message = "Stopping in #{i}..."
        log(message)
        ElixirKit.publish("log", message)
        Process.sleep(1000)
      end

      System.stop()
    end)

    {:ok, %{ref: ref}}
  end

  @impl true
  def handle_info({:event, "log", message}, state) do
    log(message)
    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _, :shutdown}, state) when ref == state.ref do
    System.stop()
    {:stop, :shutdown, state}
  end

  @impl true
  def terminate(_reason, _state) do
    log("Stopping...")
  end

  defp log(message) do
    IO.puts(["[server] ", message])
  end
end

defmodule Demo.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [Demo.Server]
    opts = [strategy: :one_for_one, name: Demo.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

defmodule Demo.Server do
  use GenServer

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg)
  end

  @impl true
  def init(_) do
    Task.start_link(fn ->
      for i <- 10..1//-1 do
        IO.puts("Quitting in #{i}...")
        Process.sleep(1000)
      end

      System.halt(0)
    end)

    {:ok, nil}
  end
end

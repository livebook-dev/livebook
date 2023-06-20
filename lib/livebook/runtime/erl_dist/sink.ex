defmodule Livebook.Runtime.ErlDist.Sink do
  @moduledoc false

  # An idle process that ignores all incoming messages.

  use GenServer

  @name __MODULE__

  @doc """
  Starts the process.
  """
  @spec start_link() :: GenServer.on_start()
  def start_link() do
    GenServer.start_link(__MODULE__, {}, name: @name)
  end

  @doc """
  Returns pid of the global sink process.
  """
  @spec pid() :: pid()
  def pid() do
    Process.whereis(@name)
  end

  @impl true
  def init({}) do
    {:ok, {}}
  end

  @impl true
  def handle_info(_message, state) do
    {:noreply, state}
  end
end

defmodule LiveBook.Session do
  @moduledoc false

  # Server corresponding to a single notebook session.
  #
  # The process keeps the current notebook state and serves
  # as a source of truth that multiple clients talk to.
  # Receives update requests from the clients and notifies
  # them of any changes applied to the notebook.

  use GenServer, restart: :temporary

  @typedoc """
  An id assigned to every running session process.
  """
  @type session_id :: LiveBook.Utils.id()

  ## API

  @doc """
  Starts the server process and registers it globally using the `:global` module,
  so that it's identifiable by the given id.
  """
  @spec start_link(session_id()) :: GenServer.on_start()
  def start_link(session_id) do
    GenServer.start_link(__MODULE__, [session_id: session_id], name: name(session_id))
  end

  defp name(session_id) do
    {:global, {:session, session_id}}
  end

  @doc """
  Synchronously stops the server.
  """
  @spec stop(session_id()) :: :ok
  def stop(session_id) do
    GenServer.stop(name(session_id))
  end

  ## Callbacks

  @impl true
  def init(session_id: _id) do
    {:ok, %{}}
  end
end

defmodule Livebook.Session.Worker do
  @moduledoc false

  # A dedicated process for offloading the session process,
  # when the session state is not necessary.
  #
  # In particular, this process handles broadcast messages
  # sent from within the runtime and distributes them to the
  # actual subscribers via pubsub.

  use GenServer

  def start_link(session_id) do
    GenServer.start_link(__MODULE__, {session_id})
  end

  @impl true
  def init({session_id}) do
    {:ok, %{session_id: session_id}}
  end

  @impl true
  def handle_info({:runtime_broadcast, topic, subtopic, message}, state) do
    Livebook.Session.broadcast_runtime_event(state.session_id, topic, subtopic, message)
    {:noreply, state}
  end
end

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
    full_topic = Livebook.Session.runtime_messages_topic(state.session_id, topic, subtopic)
    Phoenix.PubSub.broadcast(Livebook.PubSub, full_topic, message)
    {:noreply, state}
  end
end

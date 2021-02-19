defmodule LiveBookWeb.SessionsLive do
  use LiveBookWeb, :live_view

  alias LiveBook.SessionSupervisor

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions")
    end

    session_ids = LiveBook.SessionSupervisor.get_session_ids()

    {:ok, assign(socket, session_ids: session_ids)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="container max-w-screen-md p-4 mx-auto">
      <div class="flex flex-col shadow-md rounded-md px-3 py-2 mb-4">
        <div class="text-gray-700 text-lg font-semibold p-2">
          Sessions
        </div>
        <%= for session_id <- Enum.sort(@session_ids) do %>
          <div class="p-3 flex">
            <div class="flex-grow text-lg text-gray-500 hover:text-current">
              <%= live_redirect session_id, to: Routes.session_path(@socket, :page, session_id) %>
            </div>
            <div>
              <button phx-click="delete_session" phx-value-id="<%= session_id %>" aria-label="delete" class="text-gray-500 hover:text-current">
                <%= Icons.svg(:trash, class: "h-6") %>
              </button>
            </div>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("delete_session", %{"id" => session_id}, socket) do
    SessionSupervisor.delete_session(session_id)

    {:noreply, socket}
  end

  @impl true
  def handle_info({:session_created, id}, socket) do
    session_ids = [id | socket.assigns.session_ids]

    {:noreply, assign(socket, :session_ids, session_ids)}
  end

  def handle_info({:session_deleted, id}, socket) do
    session_ids = List.delete(socket.assigns.session_ids, id)

    {:noreply, assign(socket, :session_ids, session_ids)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}
end

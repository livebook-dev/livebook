defmodule LiveBookWeb.SessionsLive do
  use LiveBookWeb, :live_view

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
      <div class="flex flex-col shadow-md rounded px-3 py-2 mb-4">
        <div class="text-gray-700 text-lg font-semibold p-2">
          Sessions
        </div>
        <%= for session_id <- Enum.sort(@session_ids) do %>
          <div class="p-3 flex">
            <div class="flex-grow text-lg hover:opacity-70">
              <%= live_redirect session_id, to: Routes.live_path(@socket, LiveBookWeb.SessionLive, session_id) %>
            </div>
            <div>
              <button phx-click="delete_session" phx-value-id="<%= session_id %>" aria-label="delete">
                <svg class="h-6 w-6 hover:opacity-70" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16" />
                </svg>
              </button>
            </div>
          </div>
        <% end %>
      </div>
      <button phx-click="create_session" class="text-base font-medium rounded py-2 px-3 bg-purple-400 text-white shadow-md focus">
        New session
      </button>
    </div>
    """
  end

  @impl true
  def handle_event("create_session", _params, socket) do
    case LiveBook.SessionSupervisor.create_session() do
      {:ok, id} ->
        {:noreply,
         push_redirect(socket, to: Routes.live_path(socket, LiveBookWeb.SessionLive, id))}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to create a notebook: #{reason}")}
    end
  end

  def handle_event("delete_session", %{"id" => session_id}, socket) do
    LiveBook.SessionSupervisor.delete_session(session_id)

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
end

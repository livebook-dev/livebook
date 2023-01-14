defmodule LivebookWeb.HomeLive.RecentlyOpenedSessionListComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, socket}
  end

  @impl true
  def update(assigns, socket) do
    sessions = Map.fetch!(assigns, :sessions)
    {:ok, assign(socket, sessions: sessions)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <form>
      <div class="mb-4 flex items-center md:items-end justify-between">
        <div class="flex flex-row">
          <h2 class="uppercase font-semibold text-gray-500 text-sm md:text-base">
            Recently opened sessions (<%= length(@sessions) %>)
          </h2>
        </div>
      </div>
      <.session_list sessions={@sessions} socket={@socket} />
    </form>
    """
  end

  defp session_list(%{sessions: []} = assigns) do
    ~H"""
    <div class="mt-4 p-5 flex space-x-4 items-center border border-gray-200 rounded-lg">
      <div>
        <.remix_icon icon="windy-line" class="text-gray-400 text-xl" />
      </div>
      <div class="grow flex items-center justify-between">
        <div class="text-gray-600">
          You do not have any recently opened sessions.
        </div>
        <button class="button-base button-blue" phx-click="new">
          New notebook
        </button>
      </div>
    </div>
    """
  end

  defp session_list(assigns) do
    ~H"""
    <div class="flex flex-col" role="group" aria-label="running sessions list">
      <%= for session <- @sessions do %>
        <div class="py-4 flex items-center border-b border-gray-300" data-test-session-id={session.id}>
          <div class="grow flex flex-col items-start">
            <%= live_redirect(session.notebook_name,
              to: Routes.session_path(@socket, :page, session.id),
              class: "font-semibold text-gray-800 hover:text-gray-900"
            ) %>
            <div class="text-gray-600 text-sm">
              <%= if session.file, do: session.file.path, else: "No file" %>
            </div>
          </div>
        </div>
      <% end %>
    </div>
    """
  end
end

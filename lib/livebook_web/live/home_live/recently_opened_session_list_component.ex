defmodule LivebookWeb.HomeLive.RecentlyOpenedSessionListComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.SessionHelpers

  alias Livebook.{Session, Notebook}

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
      <.session_list sessions={@sessions} socket={@socket} myself={@myself} />
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
          <div class="mx-4 mr-2 text-gray-600 flex flex-row gap-1">
            <button
              class="w-28 button-base button-outlined-gray px-4 pl-2 py-1"
              type="button"
              aria-label="toggle edit"
              phx-click="fork_session"
              phx-target={@myself}
              phx-value-id={session.id}
            >
              <.remix_icon icon="git-branch-line" />
              <span>Fork</span>
            </button>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_event("fork_session", %{"id" => session_id}, socket) do
    session = Enum.find(socket.assigns.sessions, &(&1.id == session_id))
    %{images_dir: images_dir} = session
    data = Session.get_data(session.pid)
    notebook = Notebook.forked(data.notebook)

    origin =
      if data.file do
        {:file, data.file}
      else
        data.origin
      end

    {:noreply,
     create_session(socket,
       notebook: notebook,
       copy_images_from: images_dir,
       origin: origin
     )}
  end
end

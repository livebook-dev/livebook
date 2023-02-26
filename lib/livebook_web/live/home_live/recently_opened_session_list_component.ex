defmodule LivebookWeb.HomeLive.SessionManagerSessionListComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.SessionHelpers

  alias Livebook.{Session, Notebook, FileSystem, LiveMarkdown}

  @impl true
  def mount(socket) do
    {:ok, socket}
  end

  @impl true
  def update(assigns, socket) do
    session_paths = Map.fetch!(assigns, :session_paths)
    sessions = Map.fetch!(assigns, :sessions)
    {:ok, assign(socket, session_paths: session_paths, sessions: sessions)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <form>
      <div class="mb-4 flex items-center md:items-end justify-between">
        <div class="flex flex-row">
          <h2 class="uppercase font-semibold text-gray-500 text-sm md:text-base">
            Recently opened sessions (<%= length(@session_paths) %>)
          </h2>
        </div>
      </div>
      <.session_list session_paths={@session_paths} socket={@socket} myself={@myself} />
    </form>
    """
  end

  defp session_list(%{session_paths: []} = assigns) do
    ~H"""
    <div class="mt-4 p-5 flex space-x-4 items-center border border-gray-200 rounded-lg">
      <div>
        <.remix_icon icon="windy-line" class="text-gray-400 text-xl" />
      </div>
      <div class="grow flex items-center justify-between">
        <div class="text-gray-600">
          You do not have any recently opened sessions.
        </div>
      </div>
    </div>
    """
  end

  defp session_list(assigns) do
    ~H"""
    <div class="flex flex-col" role="group" aria-label="running sessions list">
      <%= for session_path <- @session_paths do %>
        <div class="py-4 flex items-center border-b border-gray-300">
          <div class="grow flex flex-col items-start">
            <a
              class="text-gray-600 text-sm"
              href="#"
              phx-click="open"
              phx-target={@myself}
              phx-value-path={session_path}
            >
              <%= session_path %>
            </a>
          </div>
          <div class="mx-4 mr-2 text-gray-600 flex flex-row gap-1">
            <button
              class="w-28 button-base button-outlined-gray px-4 pl-2 py-1"
              type="button"
              aria-label="toggle edit"
              phx-click="fork_session"
              phx-target={@myself}
              phx-value-path={session_path}
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
  def handle_event("open", %{"path" => path}, socket) do
    path = Path.expand(path)
    file = FileSystem.File.local(path)

    if file_running?(file, socket.assigns.sessions) do
      session_id = session_id_by_file(file, socket.assigns.sessions)
      {:noreply, push_navigate(socket, to: ~p"/sessions/#{session_id}")}
    else
      {:noreply, open_notebook(socket, FileSystem.File.local(path))}
    end
  end

  def handle_event("fork_session", %{"path" => path}, socket) do
    path = Path.expand(path)
    file = FileSystem.File.local(path)

    socket =
      case import_notebook(file) do
        {:ok, {notebook, messages}} ->
          notebook = Notebook.forked(notebook)
          images_dir = Session.images_dir_for_notebook(file)

          socket
          |> put_import_warnings(messages)
          |> create_session(
            notebook: notebook,
            copy_images_from: images_dir,
            origin: {:file, file}
          )

        {:error, error} ->
          Session.SessionManager.delete_recently_opened_sessions(file.path)
          put_flash(socket, :error, Livebook.Utils.upcase_first(error))
      end

    {:noreply, socket}
  end

  defp file_running?(file, sessions) do
    running_files = files(sessions)
    file in running_files
  end

  defp files(sessions) do
    Enum.map(sessions, & &1.file)
  end

  defp session_id_by_file(file, sessions) do
    session = Enum.find(sessions, &(&1.file == file))
    session.id
  end

  defp open_notebook(socket, file) do
    case import_notebook(file) do
      {:ok, {notebook, messages}} ->
        socket
        |> put_import_warnings(messages)
        |> create_session(notebook: notebook, file: file, origin: {:file, file})

      {:error, error} ->
        Session.SessionManager.delete_recently_opened_sessions(file.path)
        put_flash(socket, :error, Livebook.Utils.upcase_first(error))
    end
  end

  defp import_notebook(file) do
    with {:ok, content} <- FileSystem.File.read(file) do
      {:ok, LiveMarkdown.notebook_from_livemd(content)}
    end
  end
end

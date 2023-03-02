defmodule LivebookWeb.HomeLive.RecentNotebookListComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.SessionHelpers

  alias Livebook.{Session, Notebook, FileSystem, LiveMarkdown}

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <div class="mb-4 flex items-center md:items-end justify-between">
        <h2 class="uppercase font-semibold text-gray-500 text-sm md:text-base">
          Recent notebooks
        </h2>
      </div>
      <.notebook_list recent_notebooks={@recent_notebooks} sessions={@sessions} myself={@myself} />
    </div>
    """
  end

  defp notebook_list(assigns) do
    ~H"""
    <div
      class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-5"
      role="group"
      aria-label="recent notebooks"
    >
      <%= for {info, idx} <- Enum.with_index(@recent_notebooks) do %>
        <div class="flex flex-col p-5 border bg-gray-50 border-gray-300 rounded-lg">
          <div class="flex items-center justify-between">
            <span class="text-gray-800 text-lg font-medium">
              <%= info.name %>
            </span>
            <.remix_icon icon="history-line" class="text-gray-800" />
          </div>
          <div class="mt-1 flex-grow text-gray-600 text-sm">
            Opened <%= format_date_relatively(info.added_at) %>
          </div>
          <div class="mt-2 flex space-x-6">
            <%= if file_running?(info.file, @sessions) do %>
              <.link
                navigate={~p"/sessions/#{session_id_by_file(info.file, @sessions)}"}
                class="text-blue-600 font-medium"
              >
                Join session
              </.link>
            <% else %>
              <button
                class="text-blue-600 font-medium"
                phx-click={JS.push("open", value: %{idx: idx}, target: @myself)}
              >
                Open
              </button>
            <% end %>
            <button
              class="text-blue-600 font-medium"
              phx-click={JS.push("fork", value: %{idx: idx}, target: @myself)}
            >
              Fork
            </button>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  defp format_date_relatively(date) do
    time_words = date |> DateTime.to_naive() |> Livebook.Utils.Time.time_ago_in_words()
    time_words <> " ago"
  end

  @impl true
  def handle_event("open", %{"idx" => idx}, socket) do
    %{file: file} = Enum.at(socket.assigns.recent_notebooks, idx)

    if file_running?(file, socket.assigns.sessions) do
      session_id = session_id_by_file(file, socket.assigns.sessions)
      {:noreply, push_navigate(socket, to: ~p"/sessions/#{session_id}")}
    else
      {:noreply, open_notebook(socket, file)}
    end
  end

  def handle_event("fork", %{"idx" => idx}, socket) do
    %{file: file} = Enum.at(socket.assigns.recent_notebooks, idx)

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
    # TODO can't use flash because it doesn't show up in a live component
    case import_notebook(file) do
      {:ok, {notebook, messages}} ->
        socket
        |> put_import_warnings(messages)
        |> create_session(notebook: notebook, file: file, origin: {:file, file})

      {:error, error} ->
        put_flash(socket, :error, Livebook.Utils.upcase_first(error))
    end
  end

  defp import_notebook(file) do
    with {:ok, content} <- FileSystem.File.read(file) do
      {:ok, LiveMarkdown.notebook_from_livemd(content)}
    end
  end
end

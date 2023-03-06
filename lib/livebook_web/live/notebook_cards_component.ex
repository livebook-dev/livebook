defmodule LivebookWeb.NotebookCardsComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.SessionHelpers

  alias Livebook.{Session, Notebook, FileSystem, LiveMarkdown}

  @impl true
  def render(assigns) do
    assigns = assign_new(assigns, :card_icon, fn -> nil end)

    ~H"""
    <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-5" role="group">
      <%= for {info, idx} <- Enum.with_index(@notebook_infos) do %>
        <div class="flex flex-col p-5 border bg-gray-50 border-gray-300 rounded-lg">
          <div class="flex items-center justify-between">
            <span class="tooltip top" data-tooltip={info.file.path}>
              <span class="text-gray-800 text-lg font-medium">
                <%= info.name %>
              </span>
            </span>
            <%= @card_icon && render_slot(@card_icon) %>
          </div>
          <div class="mt-1 flex-grow text-gray-600 text-sm">
            <%= @added_at_label %> <%= format_date_relatively(info.added_at) %>
          </div>
          <div class="mt-2 flex space-x-6">
            <%= if session = session_by_file(info.file, @sessions) do %>
              <.link navigate={~p"/sessions/#{session.id}"} class="text-blue-600 font-medium">
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
    %{file: file} = Enum.at(socket.assigns.notebook_infos, idx)

    if file_running?(file, socket.assigns.sessions) do
      session = session_by_file(file, socket.assigns.sessions)
      {:noreply, push_navigate(socket, to: ~p"/sessions/#{session.id}")}
    else
      {:noreply, open_notebook(socket, file)}
    end
  end

  def handle_event("fork", %{"idx" => idx}, socket) do
    %{file: file} = Enum.at(socket.assigns.notebook_infos, idx)

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
    Enum.any?(sessions, &(&1.file == file))
  end

  defp session_by_file(file, sessions) do
    Enum.find(sessions, &(&1.file == file))
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

defmodule LivebookWeb.OpenLive.FileComponent do
  use LivebookWeb, :live_component

  alias Livebook.LiveMarkdown
  alias Livebook.FileSystem

  @impl true
  def mount(socket) do
    {:ok, assign(socket, file_info: %{exists: true, access: :read_write})}
  end

  @impl true
  def update(%{event: {:mount_file_system, file_system}}, socket) do
    :ok = FileSystem.mount(file_system)
    {:ok, socket}
  end

  def update(%{event: {:set_file, file, info}}, socket) do
    file_info = %{exists: info.exists, access: file_access(file)}
    {:ok, assign(socket, file: file, file_info: file_info)}
  end

  def update(assigns, socket) do
    {initial_file, assigns} = Map.pop!(assigns, :initial_file)

    {:ok,
     socket
     |> assign_new(:file, fn -> initial_file end)
     |> assign(assigns)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="h-80" role="region" aria-label="file storage">
      <.live_component
        module={LivebookWeb.FileSelectComponent}
        id="home-file-select"
        file={@file}
        extnames={[LiveMarkdown.extension()]}
        running_files={files(@sessions)}
        writable={writable?(@file_info)}
        target={{__MODULE__, @id}}
        file_systems={@file_systems}
      >
        <div class="flex justify-end space-x-2">
          <.button
            color="gray"
            outlined
            phx-click="fork"
            phx-target={@myself}
            disabled={not path_forkable?(@file, @file_info)}
          >
            <.remix_icon icon="git-branch-line" />
            <span>Fork</span>
          </.button>
          <%= if session = session_by_file(@file, @sessions)  do %>
            <.button navigate={~p"/sessions/#{session.id}"}>
              Join session
            </.button>
          <% else %>
            <span {open_button_tooltip_attrs(@file, @file_info)}>
              <.button
                phx-click="open"
                phx-target={@myself}
                disabled={not path_openable?(@file, @file_info, @sessions)}
              >
                Open
              </.button>
            </span>
          <% end %>
        </div>
      </.live_component>
    </div>
    """
  end

  defp open_button_tooltip_attrs(file, file_info) do
    if regular?(file, file_info) and not writable?(file_info) do
      [
        class: "tooltip top",
        "data-tooltip": "This file is write-protected, fork to create an editable copy"
      ]
    else
      []
    end
  end

  @impl true
  def handle_event("fork", %{}, socket) do
    send(self(), {:fork, socket.assigns.file})
    {:noreply, socket}
  end

  def handle_event("open", %{}, socket) do
    send(self(), {:open, socket.assigns.file})
    {:noreply, socket}
  end

  defp path_forkable?(file, file_info) do
    regular?(file, file_info)
  end

  defp path_openable?(file, file_info, sessions) do
    regular?(file, file_info) and not file_running?(file, sessions) and
      writable?(file_info)
  end

  defp regular?(file, file_info) do
    file_info.exists and not FileSystem.File.dir?(file)
  end

  defp writable?(file_info) do
    file_info.access in [:read_write, :write]
  end

  defp file_access(file) do
    case FileSystem.File.access(file) do
      {:ok, access} -> access
      {:error, _} -> :none
    end
  end

  defp file_running?(file, sessions) do
    Enum.any?(sessions, &(&1.file && FileSystem.File.equal?(&1.file, file)))
  end

  defp files(sessions) do
    for session <- sessions, session.file, do: session.file
  end

  defp session_by_file(file, sessions) do
    Enum.find(sessions, &(&1.file && FileSystem.File.equal?(&1.file, file)))
  end
end

defmodule LivebookWeb.SessionLive.PersistenceComponent do
  use LivebookWeb, :live_component

  alias Livebook.{Session, SessionSupervisor, LiveMarkdown}

  @impl true
  def mount(socket) do
    session_summaries = SessionSupervisor.get_session_summaries()
    running_paths = Enum.map(session_summaries, & &1.path)
    {:ok, assign(socket, running_paths: running_paths)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="p-6 pb-4 max-w-4xl w-screen flex flex-col space-y-3">
      <h3 class="text-2xl font-semibold text-gray-800">
        File
      </h3>
      <div class="w-full flex-col space-y-5">
        <p class="text-gray-700">
          Specify where the notebook should be automatically persisted.
        </p>
        <div class="flex space-x-4">
          <%= content_tag :button, "Save to file",
            class: "choice-button #{if(@path != nil, do: "active")}",
            phx_click: "set_persistence_type",
            phx_value_type: "file",
            phx_target: @myself %>
          <%= content_tag :button, "Memory only",
            class: "choice-button #{if(@path == nil, do: "active")}",
            phx_click: "set_persistence_type",
            phx_value_type: "memory",
            phx_target: @myself %>
        </div>
        <%= if @path != nil do %>
          <div class="h-full h-52">
            <%= live_component @socket, LivebookWeb.PathSelectComponent,
              id: "path_select",
              path: @path,
              extnames: [LiveMarkdown.extension()],
              running_paths: @running_paths,
              phx_target: @myself,
              phx_submit: if(disabled?(@path, @current_path, @running_paths), do: nil, else: "save") %>
          </div>
        <% end %>
        <div class="flex flex-col space-y-2">
          <%= if @path != nil do %>
            <div class="text-gray-500 text-sm">
              File: <%= normalize_path(@path) %>
            </div>
          <% end %>
          <div>
            <%= content_tag :button, "Save",
              class: "button button-blue mt-2",
              phx_click: "save",
              phx_target: @myself,
              disabled: disabled?(@path, @current_path, @running_paths) %>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("set_persistence_type", %{"type" => type}, socket) do
    path =
      case type do
        "file" -> socket.assigns.current_path || default_path()
        "memory" -> nil
      end

    {:noreply, assign(socket, path: path)}
  end

  def handle_event("set_path", %{"path" => path}, socket) do
    {:noreply, assign(socket, path: path)}
  end

  def handle_event("save", %{}, socket) do
    path = normalize_path(socket.assigns.path)
    Session.set_path(socket.assigns.session_id, path)
    Session.save_sync(socket.assigns.session_id)

    running_paths =
      if path do
        [path | socket.assigns.running_paths]
      else
        List.delete(socket.assigns.running_paths, path)
      end

    # After saving the file reload the directory contents,
    # so that the new file gets shown.
    send_update(LivebookWeb.PathSelectComponent,
      id: "path_select",
      running_paths: running_paths,
      force_reload: true
    )

    {:noreply, assign(socket, running_paths: running_paths)}
  end

  defp default_path() do
    Livebook.Config.root_path() |> Path.join("notebook")
  end

  defp path_savable?(nil, _running_paths), do: true

  defp path_savable?(path, running_paths) do
    if File.exists?(path) do
      File.regular?(path) and path not in running_paths
    else
      true
    end
  end

  defp normalize_path(nil), do: nil

  defp normalize_path(path) do
    if String.ends_with?(path, LiveMarkdown.extension()) do
      path
    else
      path <> LiveMarkdown.extension()
    end
  end

  defp disabled?(path, current_path, running_paths) do
    not path_savable?(normalize_path(path), running_paths) or normalize_path(path) == current_path
  end
end

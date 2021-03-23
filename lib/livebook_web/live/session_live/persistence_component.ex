defmodule LivebookWeb.SessionLive.PersistenceComponent do
  use LivebookWeb, :live_component

  alias Livebook.{Session, SessionSupervisor, LiveMarkdown}

  @impl true
  def mount(socket) do
    session_summaries = SessionSupervisor.get_session_summaries()
    {:ok, assign(socket, session_summaries: session_summaries)}
  end

  @impl true
  def render(assigns) do
    ~L"""
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
            running_paths: paths(@session_summaries),
            target: @myself %>
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
            class: "button button-primary",
            phx_click: "save",
            phx_target: @myself,
            disabled: not path_savable?(normalize_path(@path), @session_summaries) or normalize_path(@path) == @current_path %>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("set_persistence_type", %{"type" => type}, socket) do
    path =
      case type do
        "file" -> default_path()
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
    {:noreply, socket}
  end

  defp default_path() do
    File.cwd!() |> Path.join("notebook")
  end

  defp paths(session_summaries) do
    Enum.map(session_summaries, & &1.path)
  end

  defp path_savable?(nil, _session_summaries), do: true

  defp path_savable?(path, session_summaries) do
    if File.exists?(path) do
      running_paths = paths(session_summaries)
      File.regular?(path) and path not in running_paths
    else
      dir = Path.dirname(path)
      File.exists?(dir)
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
end

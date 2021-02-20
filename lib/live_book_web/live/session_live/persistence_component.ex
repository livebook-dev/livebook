defmodule LiveBookWeb.SessionLive.PersistenceComponent do
  use LiveBookWeb, :live_component

  alias LiveBook.{Session, SessionSupervisor, LiveMarkdown}

  @impl true
  def mount(socket) do
    session_summaries = SessionSupervisor.get_session_summaries()
    {:ok, assign(socket, session_summaries: session_summaries)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="p-6 pb-4 max-w-4xl w-screen flex flex-col space-y-4">
      <h3 class="text-lg font-medium text-gray-900">
        Configure file
      </h3>
      <div class="w-full flex-col space-y-3">
        <p class="text-gray-500">
          Specify where the notebook should be automatically persisted.
        </p>
        <div>
          <form phx-change="set_persistence_type" phx-target="<%= @myself %>">
            <div class="radio-button-group">
              <label class="radio-button">
                <%= tag :input, class: "radio-button__input", type: "radio", name: "type", value: "file", checked: @path != nil %>
                <span class="radio-button__label">Save to file</span>
              </label>
              <label class="radio-button">
                <%= tag :input, class: "radio-button__input", type: "radio", name: "type", value: "memory", checked: @path == nil %>
                <span class="radio-button__label">Memory only</span>
              </label>
            </div>
          </form>
        </div>
        <%= if @path != nil do %>
          <div class="w-full container flex flex-col space-y-4">
            <%= live_component @socket, LiveBookWeb.PathSelectComponent,
              id: "path_select",
              path: @path,
              running_paths: paths(@session_summaries),
              target: @myself %>
          </div>
          <div class="text-gray-500 text-sm">
            <%= normalize_path(@path) %>
          </div>
        <% end %>
        </div>
      <div class="flex justify-end">
        <%= content_tag :button, "Done",
          class: "button-base button-primary button-sm",
          phx_click: "done",
          phx_target: @myself,
          disabled: not path_savable?(normalize_path(@path), @session_summaries) %>
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

  def handle_event("done", %{}, socket) do
    path = normalize_path(socket.assigns.path)
    Session.set_path(socket.assigns.session_id, path)

    {:noreply,
     push_patch(socket, to: Routes.session_path(socket, :page, socket.assigns.session_id))}
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

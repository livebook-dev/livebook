defmodule LiveBookWeb.SettingsComponent do
  use LiveBookWeb, :live_component

  @impl true
  def mount(socket) do
    # TODO: pass path from the LV
    {:ok, assign(socket, path: nil)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="p-6 pb-4 sm:max-w-4xl sm:w-full flex flex-col space-y-4">
      <h3 class="text-lg font-medium text-gray-900">
        Settings
      </h3>
      <div class="flex-col space-y-3">
        <p class="text-gray-500">
          Choose the file to which the notebook is automatically saved.
        </p>
        <div>
          <form phx-change="toggle_file" phx-target="<%= @myself %>">
            <label class="inline-flex items-center space-x-3 cursor-pointer">
              <input class="checkbox-base" type="checkbox" <%= if(@path != nil, do: "checked") %>>
              <span>Attach to file</span>
            </label>
          </form>
        </div>
        <%= if @path != nil do %>
          <form phx-change="set_path" phx-target="<%= @myself %>">
            <input class="input-base shadow"
              type="text"
              name="path"
              placeholder="File"
              value="/home/jonatanklosko"
              spellcheck="false" />
          </form>
        <% end %>
        </div>
      <div class="flex justify-end">
        <button class="button-base button-primary button-sm"
          phx-click="done"
          phx-target="<%= @myself %>">
          Done
        </button>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("toggle_file", %{}, socket) do
    path = if(socket.assigns.path == nil, do: "/home/jonatanklosko", else: nil)
    {:noreply, assign(socket, path: path)}
  end

  def handle_event("set_path", %{"path" => path}, socket) do
    {:noreply, assign(socket, path: path)}
  end

  def handle_event("done", %{}, socket) do
    {:noreply, socket}
  end
end

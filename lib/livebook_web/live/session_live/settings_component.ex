defmodule LivebookWeb.SessionLive.SettingsComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~L"""
    <div class="p-6 pb-4 max-w-4xl w-screen flex flex-col space-y-3">
      <h3 class="text-2xl font-semibold text-gray-800">
        Notebook settings
      </h3>
      <div class="tabs">
        <%= live_patch to: Routes.session_path(@socket, :settings, @session_id, "file"),
          class: "tab #{if(@tab == "file", do: "active")}" do %>
          <%= remix_icon("file-settings-line", class: "align-middle") %>
          <span class="font-medium">
            File
          </span>
        <% end %>
        <%= live_patch to: Routes.session_path(@socket, :settings, @session_id, "runtime"),
          class: "tab #{if(@tab == "runtime", do: "active")}" do %>
          <%= remix_icon("play-circle-line", class: "align-middle") %>
          <span class="font-medium">
            Runtime
          </span>
        <% end %>
        <div class="flex-grow tab">
        </div>
      </div>
      <div>
        <%= if @tab == "file" do %>
          <%= live_component @socket, LivebookWeb.SessionLive.PersistenceComponent,
            id: :persistence,
            session_id: @session_id,
            current_path: @data.path,
            path: @data.path %>
        <% end %>
        <%= if @tab == "runtime" do %>
          <%= live_component @socket, LivebookWeb.SessionLive.RuntimeComponent,
            id: :runtime,
            session_id: @session_id,
            runtime: @data.runtime %>
        <% end %>
      </div>
    </div>
    """
  end
end

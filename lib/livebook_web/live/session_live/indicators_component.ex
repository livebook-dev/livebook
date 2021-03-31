defmodule LivebookWeb.SessionLive.IndicatorsComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex flex-col space-y-2 items-center" data-element="notebook-indicators">
      <%= if @data_view.path do %>
        <%= if @data_view.dirty do %>
          <span class="tooltip left" aria-label="Autosave pending">
            <button class="icon-button icon-outlined-button border-blue-400 hover:bg-blue-50"
              phx-click="save">
              <%= remix_icon("save-line", class: "text-xl text-blue-500") %>
            </button>
          </span>
        <% else %>
          <span class="tooltip left" aria-label="Notebook saved">
            <button class="icon-button icon-outlined-button border-green-300 hover:bg-green-50 cursor-default">
              <%= remix_icon("save-line", class: "text-xl text-green-400") %>
            </button>
          </span>
        <% end %>
      <% else %>
        <span class="tooltip left" aria-label="No file choosen">
          <%= live_patch to: Routes.session_path(@socket, :settings, @session_id, "file"),
                class: "icon-button icon-outlined-button border-gray-200 hover:bg-gray-100" do %>
            <%= remix_icon("save-line", class: "text-xl text-gray-400") %>
          <% end %>
        </span>
      <% end %>

      <%= if @data_view.runtime do %>
        <%= if @data_view.global_evaluation_status == :evaluating do %>
          <span class="tooltip left" aria-label="Evaluating">
            <button class="icon-button icon-outlined-button border-blue-400 hover:bg-blue-50 cursor-default">
              <%= remix_icon("loader-3-line", class: "text-xl text-blue-500 animate-spin") %>
            </button>
          </span>
        <% end %>
        <%= if @data_view.global_evaluation_status == :evaluated do %>
          <span class="tooltip left" aria-label="Everything evaluated">
            <button class="icon-button icon-outlined-button border-green-300 hover:bg-green-50 cursor-default">
              <%= remix_icon("loader-3-line", class: "text-xl text-green-400") %>
            </button>
          </span>
        <% end %>
        <%= if @data_view.global_evaluation_status == :ready do %>
          <span class="tooltip left" aria-label="Ready to evaluate">
            <button class="icon-button icon-outlined-button border-yellow-200 hover:bg-yellow-50"
              phx-click="queue_all_cells_evaluation">
              <%= remix_icon("loader-3-line", class: "text-xl text-yellow-300") %>
            </button>
          </span>
        <% end %>
      <% else %>
        <span class="tooltip left" aria-label="No runtime started">
          <%= live_patch to: Routes.session_path(@socket, :settings, @session_id, "runtime"),
                class: "icon-button icon-outlined-button border-gray-200 hover:bg-gray-100" do %>
            <%= remix_icon("loader-3-line", class: "text-xl text-gray-400") %>
          <% end %>
        </span>
      <% end %>

      <%# Note: this indicator is shown/hidden using CSS based on the current mode %>
      <span class="tooltip left" aria-label="Insert mode" data-element="insert-mode-indicator">
        <span class="text-sm text-gray-400 font-medium cursor-default">
          ins
        </span>
      </span>
    </div>
    """
  end
end

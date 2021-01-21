defmodule LiveBookWeb.Cell do
  use LiveBookWeb, :live_component

  def render(assigns) do
    ~L"""
    <div phx-click="focus_cell"
         phx-value-cell_id="<%= @cell.id %>"
         class="flex flex-col relative mr-10 border-2 border-gray-200 rounded border-opacity-0 <%= if @focused, do: "border-opacity-100"%>">
      <div class="flex flex-col items-center space-y-2 absolute right-0 top-0 -mr-10">
        <button class="text-gray-500 hover:text-current">
          <%= Icons.svg(:play, class: "h-6") %>
        </button>
        <button phx-click="delete_cell" phx-value-cell_id="<%= @cell.id %>" class="text-gray-500 hover:text-current">
          <%= Icons.svg(:trash, class: "h-6") %>
        </button>
      </div>
      <div
        id="cell-<%= @cell.id %>-editor"
        phx-hook="Editor"
        phx-update="ignore"
        data-cell-id="<%= @cell.id %>"
        data-type="<%= @cell.type %>">
        <div data-source="<%= @cell.source %>"
             data-revision="<%= @cell_info.revision %>">
        </div>
      </div>
    </div>
    """
  end
end

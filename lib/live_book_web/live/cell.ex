defmodule LiveBookWeb.Cell do
  use LiveBookWeb, :live_component

  def render(assigns) do
    ~L"""
    <div phx-click="focus_cell"
         phx-value-cell_id="<%= @cell.id %>"
         class="flex flex-col p-2 relative mr-10 border-2 border-gray-200 rounded border-opacity-0 <%= if @focused, do: "border-opacity-100"%>">
      <div class="flex flex-col items-center space-y-2 absolute right-0 top-0 -mr-10">
        <button class="text-gray-500 hover:text-current">
          <%= Icons.svg(:play, class: "h-6") %>
        </button>
        <button phx-click="delete_cell" phx-value-cell_id="<%= @cell.id %>" class="text-gray-500 hover:text-current">
          <%= Icons.svg(:trash, class: "h-6") %>
        </button>
      </div>
      <div
        id="cell-<%= @cell.id %>"
        phx-hook="Editor"
        phx-update="ignore"
        data-id="<%= @cell.id %>"
        data-type="<%= @cell.type %>"
      >
        <div class="h-20 flex opacity-20">
          <%= @cell.type |> Atom.to_string() |> String.capitalize() %> cell placeholder
        </div>
      </div>
    </div>
    """
  end
end

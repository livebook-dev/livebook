defmodule LivebookWeb.InsertCellComponent do
  use LivebookWeb, :live_component

  def render(assigns) do
    ~L"""
    <div class="relative top-0.5 m-0 flex justify-center">
      <div class="absolute z-10 <%= if(@persistent, do: "opacity-100", else: "opacity-0") %> hover:opacity-100 focus-within:opacity-100 flex space-x-2 justify-center items-center">
        <button class="py-1 px-2 text-sm text-gray-600 font-medium rounded-lg border border-gray-200 bg-gray-50 hover:bg-gray-100 focus:bg-gray-100"
          phx-click="insert_cell"
          phx-value-type="markdown"
          phx-value-section_id="<%= @section_id %>"
          phx-value-index="<%= @index %>">
          + Markdown
        </button>
        <button class="py-1 px-2 text-sm text-gray-600 font-medium rounded-lg border border-gray-200 bg-gray-50 hover:bg-gray-100 focus:bg-gray-100"
          phx-click="insert_cell"
          phx-value-type="elixir"
          phx-value-section_id="<%= @section_id %>"
          phx-value-index="<%= @index %>">
          + Elixir
        </button>
      </div>
    </div>
    """
  end
end

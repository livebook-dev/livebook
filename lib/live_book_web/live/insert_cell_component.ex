defmodule LivebookWeb.InsertCellComponent do
  use LivebookWeb, :live_component

  def render(assigns) do
    ~L"""
    <div class="<%= if(@persistent, do: "opacity-100", else: "opacity-0") %> hover:opacity-100 flex space-x-2 justify-center items-center">
      <%= line() %>
      <button class="py-1 px-2 rounded-md text-sm hover:bg-gray-50 border border-gray-200"
        phx-click="insert_cell"
        phx-value-type="markdown"
        phx-value-section_id="<%= @section_id %>"
        phx-value-index="<%= @index %>">
        + Markdown
      </button>
      <button class="py-1 px-2 rounded-md text-sm hover:bg-gray-50 border border-gray-200"
        phx-click="insert_cell"
        phx-value-type="elixir"
        phx-value-section_id="<%= @section_id %>"
        phx-value-index="<%= @index %>">
        + Elixir
      </button>
      <%= line() %>
    </div>
    """
  end

  defp line() do
    assigns = %{}

    ~L"""
    <div class="border-t-2 border-dashed border-gray-200 flex-grow"></div>
    """
  end
end

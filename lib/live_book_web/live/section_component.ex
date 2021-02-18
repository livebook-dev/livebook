defmodule LiveBookWeb.SectionComponent do
  use LiveBookWeb, :live_component

  def render(assigns) do
    ~L"""
    <div class="<%= if not @selected, do: "hidden" %>">
      <div class="flex space-x-4 items-center">
        <div class="flex flex-grow space-x-2 items-center text-gray-600">
          <h2 class="flex-grow text-gray-900 font-semibold text-3xl py-2 border-b-2 border-transparent hover:border-blue-100 focus:border-blue-300"
            id="section-<%= @section.id %>-name"
            contenteditable
            spellcheck="false"
            phx-blur="set_section_name"
            phx-value-section_id="<%= @section.id %>"
            phx-hook="ContentEditable"
            data-update-attribute="phx-value-name"><%= @section.name %></h2>
            <%# ^ Note it's important there's no space between <h2> and </h2>
              because we want the content to exactly match @section.name. %>
        </div>
        <div class="flex space-x-2 items-center">
          <button phx-click="delete_section" phx-value-section_id="<%= @section.id %>" class="text-gray-600 hover:text-current">
            <%= Icons.svg(:trash, class: "h-6") %>
          </button>
        </div>
      </div>
      <div class="container py-2">
        <div class="flex flex-col space-y-2 pb-80">
          <%= live_component @socket, LiveBookWeb.InsertCellComponent,
                id: "#{@section.id}:0",
                section_id: @section.id,
                index: 0,
                persistent: @section.cells == [] %>
          <%= for {cell, index} <- Enum.with_index(@section.cells) do %>
            <%= live_component @socket, LiveBookWeb.CellComponent,
                  id: cell.id,
                  cell: cell,
                  cell_info: @cell_infos[cell.id],
                  focused: @selected and cell.id == @focused_cell_id,
                  insert_mode: @insert_mode %>
            <%= live_component @socket, LiveBookWeb.InsertCellComponent,
                  id: "#{@section.id}:#{index + 1}",
                  section_id: @section.id,
                  index: index + 1,
                  persistent: false %>
          <% end %>
        </div>
      </div>
    </div>
    """
  end
end

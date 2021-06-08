defmodule LivebookWeb.SessionLive.InsertButtonsComponent do
  use LivebookWeb, :live_component

  def render(assigns) do
    ~L"""
    <div class="relative top-0.5 m-0 flex justify-center" data-element="insert-buttons">
      <div class="absolute z-10 <%= if(@persistent, do: "opacity-100", else: "opacity-0") %> hover:opacity-100 focus-within:opacity-100 flex space-x-2 justify-center items-center">
        <button class="button button-small"
          phx-click="insert_cell"
          phx-value-type="markdown"
          phx-value-section_id="<%= @section_id %>"
          phx-value-index="<%= @insert_cell_index %>"
          >+ Markdown</button>
        <button class="button button-small"
          phx-click="insert_cell"
          phx-value-type="elixir"
          phx-value-section_id="<%= @section_id %>"
          phx-value-index="<%= @insert_cell_index %>"
          >+ Elixir</button>
        <button class="button button-small"
          phx-click="insert_cell"
          phx-value-type="input"
          phx-value-section_id="<%= @section_id %>"
          phx-value-index="<%= @insert_cell_index %>"
          >+ Input</button>
        <%= if @insert_section_index do %>
          <button class="button button-small"
            phx-click="insert_section"
            phx-value-index="<%= @insert_section_index %>"
            >+ Section</button>
        <% end %>
      </div>
    </div>
    """
  end
end

defmodule LivebookWeb.SessionLive.InsertButtonsComponent do
  use LivebookWeb, :live_component

  def render(assigns) do
    ~H"""
    <div class="relative top-0.5 m-0 flex justify-center"
      role="toolbar"
      aria-label="insert new"
      data-element="insert-buttons">
      <div class={"w-full absolute z-10 focus-within:z-[11] #{if(@persistent, do: "opacity-100", else: "opacity-0")} hover:opacity-100 focus-within:opacity-100 flex space-x-2 justify-center items-center"}>
        <button class="button-base button-small"
          phx-click="insert_cell_below"
          phx-value-type="markdown"
          phx-value-section_id={@section_id}
          phx-value-cell_id={@cell_id}
          >+ Markdown</button>
        <button class="button-base button-small"
          phx-click="insert_cell_below"
          phx-value-type="code"
          phx-value-section_id={@section_id}
          phx-value-cell_id={@cell_id}
          >+ Code</button>
        <button class="button-base button-small"
          phx-click="insert_section_below"
          phx-value-section_id={@section_id}
          phx-value-cell_id={@cell_id}
          >+ Section</button>
        <%= if @smart_cell_definitions != [] do %>
          <.menu id={"#{@id}-smart-cell-menu"}>
            <:toggle>
              <button class="button-base button-small">+ Smart</button>
            </:toggle>
            <:content>
              <%= for smart_cell_definition <- Enum.sort_by(@smart_cell_definitions, & &1.name) do %>
                <button class="menu-item text-gray-500"
                  role="menuitem"
                  phx-click="insert_cell_below"
                  phx-value-type="smart"
                  phx-value-kind={smart_cell_definition.kind}
                  phx-value-section_id={@section_id}
                  phx-value-cell_id={@cell_id}>
                  <span class="font-medium"><%= smart_cell_definition.name %></span>
                </button>
              <% end %>
            </:content>
          </.menu>
        <% end %>
      </div>
    </div>
    """
  end
end

defmodule LivebookWeb.SessionLive.SectionComponent do
  use LivebookWeb, :live_component

  def render(assigns) do
    ~H"""
    <div data-element="section" data-section-id={@section_view.id}>
      <div class="flex space-x-4 items-center" data-element="section-headline">
        <h2 class="flex-grow text-gray-800 font-semibold text-2xl px-1 -ml-1 rounded-lg border
                   border-transparent hover:border-blue-200 focus:border-blue-300"
          data-element="section-name"
          id={@section_view.html_id}
          contenteditable
          spellcheck="false"
          phx-blur="set_section_name"
          phx-value-section_id={@section_view.id}
          phx-hook="ContentEditable"
          data-update-attribute="phx-value-name"><%= @section_view.name %></h2>
          <%# ^ Note it's important there's no space between <h2> and </h2>
            because we want the content to exactly match section name. %>
        <div class="flex space-x-2 items-center" data-element="section-actions">
          <span class="tooltip top" aria-label="Link">
            <a href={"##{@section_view.html_id}"} class="icon-button">
              <.remix_icon icon="link" class="text-xl" />
            </a>
          </span>
          <span class="tooltip top" aria-label="Move up">
            <button class="icon-button"
              phx-click="move_section"
              phx-value-section_id={@section_view.id}
              phx-value-offset="-1">
              <.remix_icon icon="arrow-up-s-line" class="text-xl" />
            </button>
          </span>
          <span class="tooltip top" aria-label="Move down">
            <button class="icon-button"
              phx-click="move_section"
              phx-value-section_id={@section_view.id}
              phx-value-offset="1">
              <.remix_icon icon="arrow-down-s-line" class="text-xl" />
            </button>
          </span>
          <span class="tooltip top" aria-label="Delete">
            <%= live_patch to: Routes.session_path(@socket, :delete_section, @session_id, @section_view.id),
                  class: "icon-button" do %>
              <.remix_icon icon="delete-bin-6-line" class="text-xl" />
            <% end %>
          </span>
        </div>
      </div>
      <div class="container">
        <div class="flex flex-col space-y-1">
          <%= for {cell_view, index} <- Enum.with_index(@section_view.cell_views) do %>
            <%= live_component LivebookWeb.SessionLive.InsertButtonsComponent,
                  id: "#{@section_view.id}:#{index}",
                  persistent: false,
                  section_id: @section_view.id,
                  insert_cell_index: index %>

            <%= live_component LivebookWeb.SessionLive.CellComponent,
                  id: cell_view.id,
                  session_id: @session_id,
                  cell_view: cell_view %>
          <% end %>

          <%= live_component LivebookWeb.SessionLive.InsertButtonsComponent,
                id: "#{@section_view.id}:last",
                persistent: @section_view.cell_views == [],
                section_id: @section_view.id,
                insert_cell_index: length(@section_view.cell_views) %>
        </div>
      </div>
    </div>
    """
  end
end

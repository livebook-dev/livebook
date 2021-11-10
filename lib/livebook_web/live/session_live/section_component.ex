defmodule LivebookWeb.SessionLive.SectionComponent do
  use LivebookWeb, :live_component

  def render(assigns) do
    ~H"""
    <div data-element="section" data-section-id={@section_view.id}>
      <div class="flex space-x-4 items-center" data-element="section-headline">
        <h2 class="flex-grow text-gray-800 font-semibold text-2xl px-1 -ml-1 rounded-lg border
                   border-transparent hover:border-blue-200 focus:border-blue-300"
          aria-description="section title"
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
          <span class="tooltip top" data-tooltip="Link">
            <a href={"##{@section_view.html_id}"} class="icon-button" aria-label="link to section">
              <.remix_icon icon="link" class="text-xl" />
            </a>
          </span>
          <%= if @section_view.valid_parents != [] and not @section_view.has_children? do %>
            <.menu id={"section-#{@section_view.id}-branch-menu"}>
              <:toggle>
                <span class="tooltip top" data-tooltip="Branch out from">
                  <button class="icon-button"
                    aria-label="branch out from other section">
                    <.remix_icon icon="git-branch-line" class="text-xl flip-horizontally" />
                  </button>
                </span>
              </:toggle>
              <:content>
                <%= for parent <- @section_view.valid_parents do %>
                  <%= if @section_view.parent && @section_view.parent.id == parent.id do %>
                    <button class="menu-item text-gray-900"
                      phx-click="unset_section_parent"
                      phx-value-section_id={@section_view.id}>
                      <.remix_icon icon="arrow-right-s-line" />
                      <span class="font-medium"><%= parent.name %></span>
                    </button>
                  <% else %>
                    <button class="menu-item text-gray-500"
                      phx-click="set_section_parent"
                      phx-value-section_id={@section_view.id}
                      phx-value-parent_id={parent.id}>
                      <.remix_icon icon="arrow-right-s-line" />
                      <span class="font-medium"><%= parent.name %></span>
                    </button>
                  <% end %>
                <% end %>
              </:content>
            </.menu>
          <% end %>
          <span class="tooltip top" data-tooltip="Move up">
            <button class="icon-button"
              aria-label="move section up"
              phx-click="move_section"
              phx-value-section_id={@section_view.id}
              phx-value-offset="-1">
              <.remix_icon icon="arrow-up-s-line" class="text-xl" />
            </button>
          </span>
          <span class="tooltip top" data-tooltip="Move down">
            <button class="icon-button"
              aria-label="move section down"
              phx-click="move_section"
              phx-value-section_id={@section_view.id}
              phx-value-offset="1">
              <.remix_icon icon="arrow-down-s-line" class="text-xl" />
            </button>
          </span>
          <span class="tooltip top" data-tooltip={if @section_view.has_children?, do: "Cannot delete this section because\nother sections branch from it", else: "Delete"}>
            <%= live_patch to: Routes.session_path(@socket, :delete_section, @session_id, @section_view.id),
                  class: "icon-button #{if @section_view.has_children?, do: "disabled"}",
                  aria_label: "delete section" do %>
              <.remix_icon icon="delete-bin-6-line" class="text-xl" />
            <% end %>
          </span>
        </div>
      </div>
      <%= if @section_view.parent do %>
        <h3 class="mt-1 flex items-end space-x-1 text-sm font-semibold text-gray-800">
          <span class="tooltip bottom" data-tooltip={"This section branches out from the main flow\nand can be evaluated in parallel"}>
            <.remix_icon icon="git-branch-line" class="text-lg font-normal flip-horizontally leading-none" />
          </span>
          <span class="leading-none">from ”<%= @section_view.parent.name %>”</span>
        </h3>
      <% end %>
      <div class="container">
        <div class="flex flex-col space-y-1">
          <%= for {cell_view, index} <- Enum.with_index(@section_view.cell_views) do %>
            <.live_component module={LivebookWeb.SessionLive.InsertButtonsComponent}
                id={"#{@section_view.id}:#{index}"}
                persistent={false}
                section_id={@section_view.id}
                insert_cell_index={index} />

            <.live_component module={LivebookWeb.SessionLive.CellComponent}
                id={cell_view.id}
                session_id={@session_id}
                runtime={@runtime}
                cell_view={cell_view} />
          <% end %>

          <.live_component module={LivebookWeb.SessionLive.InsertButtonsComponent}
              id={"#{@section_view.id}:last"}
              persistent={@section_view.cell_views == []}
              section_id={@section_view.id}
              insert_cell_index={length(@section_view.cell_views)} />
        </div>
      </div>
    </div>
    """
  end
end

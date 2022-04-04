defmodule LivebookWeb.SessionLive.SectionComponent do
  use LivebookWeb, :live_component

  def render(assigns) do
    ~H"""
    <section data-el-section data-section-id={@section_view.id}>
      <div class="flex space-x-4 items-center"
        data-el-section-headline
        id={@section_view.id}
        data-focusable-id={@section_view.id}
        phx-hook="Headline"
        data-on-value-change="set_section_name"
        data-metadata={@section_view.id}>
        <h2 class="grow text-gray-800 font-semibold text-2xl px-1 -ml-1 rounded-lg border border-transparent whitespace-pre-wrap cursor-text"
          tabindex="0"
          id={@section_view.html_id}
          data-el-heading
          spellcheck="false"><%= @section_view.name %></h2>
        <div class="flex space-x-2 items-center" data-el-section-actions
          role="toolbar"
          aria-label="section actions">
          <span class="tooltip top" data-tooltip="Link">
            <a href={"##{@section_view.html_id}"} class="icon-button" aria-label="link to section">
              <.remix_icon icon="link" class="text-xl" />
            </a>
          </span>
          <%= if @section_view.valid_parents != [] and not @section_view.has_children? do %>
            <.menu id={"section-#{@section_view.id}-branch-menu"}>
              <:toggle>
                <span class="tooltip top" data-tooltip="Branch out from">
                  <button class="icon-button" aria-label="branch out from other section">
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
                    <button class="menu-item text-gray-500 bg-gray-50"
                      phx-click="set_section_parent"
                      phx-value-section_id={@section_view.id}
                      phx-value-parent_id={parent.id}>
                      <%= if @section_view.parent && @section_view.parent.id do %>
                        <.remix_icon icon="arrow-right-s-line" class="invisible" />
                      <% end %>
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
          <span
            {if @section_view.has_children?,
               do: [class: "tooltip left", data_tooltip: "Cannot delete this section because\nother sections branch from it"],
               else: [class: "tooltip top", data_tooltip: "Delete"]}>
            <button class={"icon-button #{if @section_view.has_children?, do: "disabled"}"}
              aria-label="delete section"
              phx-click="delete_section"
              phx-value-section_id={@section_view.id}>
              <.remix_icon icon="delete-bin-6-line" class="text-xl" />
            </button>
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
          <.live_component module={LivebookWeb.SessionLive.InsertButtonsComponent}
              id={"insert-buttons-#{@section_view.id}-first"}
              persistent={@section_view.cell_views == []}
              smart_cell_definitions={@smart_cell_definitions}
              runtime={@runtime}
              section_id={@section_view.id}
              cell_id={nil} />
          <%= for {cell_view, index} <- Enum.with_index(@section_view.cell_views) do %>
            <.live_component module={LivebookWeb.SessionLive.CellComponent}
                id={cell_view.id}
                session_id={@session_id}
                runtime={@runtime}
                cell_view={cell_view} />
            <.live_component module={LivebookWeb.SessionLive.InsertButtonsComponent}
                id={"insert-buttons-#{@section_view.id}-#{index}"}
                persistent={false}
                smart_cell_definitions={@smart_cell_definitions}
                runtime={@runtime}
                section_id={@section_view.id}
                cell_id={cell_view.id} />
          <% end %>
        </div>
      </div>
    </section>
    """
  end
end

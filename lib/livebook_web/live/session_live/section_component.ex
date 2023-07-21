defmodule LivebookWeb.SessionLive.SectionComponent do
  use LivebookWeb, :live_component

  alias Phoenix.LiveView.JS

  def render(assigns) do
    ~H"""
    <section data-el-section data-section-id={@section_view.id}>
      <div
        class="flex items-center relative"
        data-el-section-headline
        id={@section_view.id}
        data-focusable-id={@section_view.id}
        phx-hook="Headline"
        data-on-value-change="set_section_name"
        data-metadata={@section_view.id}
      >
        <div class="absolute left-0 top-0 bottom-0 transform -translate-x-full w-10 flex justify-end items-center pr-2">
          <button
            class="icon-button"
            aria-label="collapse section"
            data-el-section-collapse-button
            phx-click={
              JS.set_attribute({"data-js-collapsed", ""},
                to: ~s/section[data-section-id="#{@section_view.id}"]/
              )
            }
          >
            <.remix_icon icon="arrow-down-s-line" class="text-xl" />
          </button>
          <button
            class="icon-button"
            aria-label="expand section"
            data-el-section-expand-button
            phx-click={
              JS.remove_attribute("data-js-collapsed",
                to: ~s/section[data-section-id="#{@section_view.id}"]/
              )
            }
          >
            <.remix_icon icon="arrow-right-s-line" class="text-xl" />
          </button>
        </div>
        <h2
          class="grow text-gray-800 font-semibold text-2xl px-1 -ml-1.5 rounded-lg border border-transparent whitespace-pre-wrap cursor-text scroll-mt-[50px] sm:scroll-mt-0"
          tabindex="0"
          id={@section_view.html_id}
          data-el-heading
          spellcheck="false"
          phx-no-format
        ><%= @section_view.name %></h2>
        <div
          class="ml-4 flex space-x-2 items-center"
          data-el-section-actions
          role="toolbar"
          aria-label="section actions"
        >
          <span class="tooltip top" data-tooltip="Link">
            <a href={"##{@section_view.html_id}"} class="icon-button" aria-label="link to section">
              <.remix_icon icon="link" class="text-xl" />
            </a>
          </span>
          <.menu
            :if={@section_view.valid_parents != [] and not @section_view.has_children?}
            id={"section-#{@section_view.id}-branch-menu"}
          >
            <:toggle>
              <span class="tooltip top" data-tooltip="Branch out from">
                <button class="icon-button" aria-label="branch out from other section">
                  <.remix_icon icon="git-branch-line" class="text-xl flip-horizontally" />
                </button>
              </span>
            </:toggle>
            <.menu_item :for={parent <- @section_view.valid_parents}>
              <%= if @section_view.parent && @section_view.parent.id == parent.id do %>
                <button
                  class="text-gray-900"
                  phx-click="unset_section_parent"
                  phx-value-section_id={@section_view.id}
                >
                  <.remix_icon icon="arrow-right-s-line" />
                  <span><%= parent.name %></span>
                </button>
              <% else %>
                <button
                  class="text-gray-500"
                  phx-click="set_section_parent"
                  phx-value-section_id={@section_view.id}
                  phx-value-parent_id={parent.id}
                >
                  <.remix_icon
                    :if={@section_view.parent && @section_view.parent.id}
                    icon="arrow-right-s-line"
                    class="invisible"
                  />
                  <span><%= parent.name %></span>
                </button>
              <% end %>
            </.menu_item>
          </.menu>
          <span class="tooltip top" data-tooltip="Move up">
            <button
              class="icon-button"
              aria-label="move section up"
              phx-click="move_section"
              phx-value-section_id={@section_view.id}
              phx-value-offset="-1"
            >
              <.remix_icon icon="arrow-up-s-line" class="text-xl" />
            </button>
          </span>
          <span class="tooltip top" data-tooltip="Move down">
            <button
              class="icon-button"
              aria-label="move section down"
              phx-click="move_section"
              phx-value-section_id={@section_view.id}
              phx-value-offset="1"
            >
              <.remix_icon icon="arrow-down-s-line" class="text-xl" />
            </button>
          </span>
          <span {if @section_view.has_children?,
               do: [class: "tooltip left", data_tooltip: "Cannot delete this section because\nother sections branch from it"],
               else: [class: "tooltip top", data_tooltip: "Delete"]}>
            <button
              class={["icon-button", @section_view.has_children? && "disabled"]}
              aria-label="delete section"
              phx-click="delete_section"
              phx-value-section_id={@section_view.id}
            >
              <.remix_icon icon="delete-bin-6-line" class="text-xl" />
            </button>
          </span>
        </div>
      </div>
      <h3
        :if={@section_view.parent}
        class="mt-1 flex items-end space-x-1 text-sm font-semibold text-gray-800"
        data-el-section-subheadline
      >
        <span
          class="tooltip bottom"
          data-tooltip="This section branches out from the main flow
    and can be evaluated in parallel"
        >
          <.remix_icon
            icon="git-branch-line"
            class="text-lg font-normal flip-horizontally leading-none"
          />
        </span>
        <span class="leading-none">from ”<%= @section_view.parent.name %>”</span>
      </h3>

      <h3
        class="mt-2 text-sm text-gray-500 cursor-default select-none"
        data-el-section-subheadline-collapsed
      >
        <%= pluralize(length(@section_view.cell_views), "cell", "cells") %> collapsed
      </h3>
      <div class="container" data-el-section-content>
        <div class="flex flex-col space-y-1">
          <.live_component
            module={LivebookWeb.SessionLive.InsertButtonsComponent}
            id={"insert-buttons-#{@section_view.id}-first"}
            persistent={@section_view.cell_views == []}
            smart_cell_definitions={@smart_cell_definitions}
            example_snippet_definitions={@example_snippet_definitions}
            runtime={@runtime}
            section_id={@section_view.id}
            cell_id={nil}
            session_id={@session_id}
            default_language={@default_language}
          />
          <%= for {cell_view, index} <- Enum.with_index(@section_view.cell_views) do %>
            <.live_component
              module={LivebookWeb.SessionLive.CellComponent}
              id={cell_view.id}
              session_id={@session_id}
              session_pid={@session_pid}
              client_id={@client_id}
              runtime={@runtime}
              installing?={@installing?}
              allowed_uri_schemes={@allowed_uri_schemes}
              cell_view={cell_view}
            />
            <.live_component
              module={LivebookWeb.SessionLive.InsertButtonsComponent}
              id={"insert-buttons-#{@section_view.id}-#{index}"}
              persistent={false}
              smart_cell_definitions={@smart_cell_definitions}
              example_snippet_definitions={@example_snippet_definitions}
              runtime={@runtime}
              section_id={@section_view.id}
              cell_id={cell_view.id}
              session_id={@session_id}
              default_language={@default_language}
            />
          <% end %>
        </div>
      </div>
    </section>
    """
  end
end

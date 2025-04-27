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
        data-p-id={hook_prop(@section_view.id)}
        data-p-on-value-change={hook_prop("set_section_name")}
        data-p-metadata={hook_prop(@section_view.id)}
      >
        <div class="absolute left-0 top-0 bottom-0 transform -translate-x-full w-10 flex justify-end items-center pr-2">
          <.icon_button
            aria-label="collapse section"
            data-el-section-collapse-button
            phx-click={
              JS.set_attribute({"data-js-collapsed", ""},
                to: ~s/section[data-section-id="#{@section_view.id}"]/
              )
            }
          >
            <.remix_icon icon="arrow-down-s-line" />
          </.icon_button>
          <.icon_button
            aria-label="expand section"
            data-el-section-expand-button
            phx-click={
              JS.remove_attribute("data-js-collapsed",
                to: ~s/section[data-section-id="#{@section_view.id}"]/
              )
            }
          >
            <.remix_icon icon="arrow-right-s-line" />
          </.icon_button>
        </div>
        <h2
          class="grow text-gray-800 font-semibold text-2xl px-1 -ml-1.5 rounded-lg border border-transparent whitespace-pre-wrap cursor-text scroll-mt-[50px] sm:scroll-mt-0"
          tabindex="0"
          id={@section_view.html_id}
          data-el-heading
          spellcheck="false"
          phx-no-format
        >{@section_view.name}</h2>
        <div
          class="ml-4 flex space-x-2 items-center"
          data-el-section-actions
          role="toolbar"
          aria-label="section actions"
        >
          <.branching_menu
            section_view={@section_view}
            scope="actions"
            position="bottom-right"
            disabled={cannot_branch_out_reason(@section_view) != nil}
          >
            <span
              class="tooltip top"
              data-tooltip={cannot_branch_out_reason(@section_view) || "Branch out from"}
            >
              <.icon_button
                disabled={cannot_branch_out_reason(@section_view)}
                aria-label="branch out from other section"
              >
                <.remix_icon icon="git-branch-line" class="flip-horizontally" />
              </.icon_button>
            </span>
          </.branching_menu>
          <span class="tooltip top" data-tooltip="Link">
            <.icon_button href={"##{@section_view.html_id}"} aria-label="link to section">
              <.remix_icon icon="link" />
            </.icon_button>
          </span>
          <span class="tooltip top" data-tooltip="Move up">
            <.icon_button
              aria-label="move section up"
              phx-click="move_section"
              phx-value-section_id={@section_view.id}
              phx-value-offset="-1"
            >
              <.remix_icon icon="arrow-up-s-line" />
            </.icon_button>
          </span>
          <span class="tooltip top" data-tooltip="Move down">
            <.icon_button
              aria-label="move section down"
              phx-click="move_section"
              phx-value-section_id={@section_view.id}
              phx-value-offset="1"
            >
              <.remix_icon icon="arrow-down-s-line" />
            </.icon_button>
          </span>
          <span {if @section_view.has_children?,
               do: [class: "tooltip left", "data-tooltip": "Cannot delete this section because\nother sections branch from it"],
               else: [class: "tooltip top", "data-tooltip": "Delete"]}>
            <.icon_button
              disabled={@section_view.has_children?}
              aria-label="delete section"
              phx-click="delete_section"
              phx-value-section_id={@section_view.id}
            >
              <.remix_icon icon="delete-bin-6-line" />
            </.icon_button>
          </span>
        </div>
      </div>
      <h3
        :if={@section_view.parent}
        class="mt-1 flex items-end space-x-1 font-semibold text-gray-800"
        data-el-section-subheadline
      >
        <div
          class="tooltip bottom"
          data-tooltip="This section branches out from the main flow
    and can be evaluated in parallel"
        >
          <.remix_icon
            icon="git-branch-line"
            class="text-lg font-normal flip-horizontally leading-none"
          />
        </div>
        <.branching_menu section_view={@section_view} scope="subheading" position="bottom-left">
          <div class="text-sm leading-none cursor-pointer">
            from ”{@section_view.parent.name}”
          </div>
        </.branching_menu>
      </h3>

      <h3
        class="mt-2 text-sm text-gray-500 cursor-default select-none"
        data-el-section-subheadline-collapsed
      >
        {LivebookWeb.HTMLHelpers.pluralize(length(@section_view.cell_views), "cell", "cells")} collapsed
      </h3>
      <div class="container" data-el-section-content>
        <div class="flex flex-col space-y-1">
          <.live_component
            module={LivebookWeb.SessionLive.InsertButtonsComponent}
            id={"insert-buttons-#{@section_view.id}-first"}
            persistent={@section_view.cell_views == []}
            smart_cell_definitions={@smart_cell_definitions}
            example_snippet_definitions={@example_snippet_definitions}
            runtime_status={@runtime_status}
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
              runtime_status={@runtime_status}
              installing?={@installing?}
              allowed_uri_schemes={@allowed_uri_schemes}
              enabled_languages={@enabled_languages}
              cell_view={cell_view}
            />
            <.live_component
              module={LivebookWeb.SessionLive.InsertButtonsComponent}
              id={"insert-buttons-#{@section_view.id}-#{index}"}
              persistent={false}
              smart_cell_definitions={@smart_cell_definitions}
              example_snippet_definitions={@example_snippet_definitions}
              runtime_status={@runtime_status}
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

  attr :section_view, :map, required: true
  attr :scope, :string, required: true
  attr :position, :string, required: true
  attr :disabled, :boolean, default: false

  slot :inner_block, required: true

  defp branching_menu(assigns) do
    ~H"""
    <.menu
      id={"section-#{@section_view.id}-branch-menu-#{@scope}"}
      position={@position}
      disabled={@disabled}
    >
      <:toggle>
        {render_slot(@inner_block)}
      </:toggle>
      <%= if @section_view.parent do %>
        <.menu_item>
          <button
            class="text-gray-500"
            phx-click="unset_section_parent"
            phx-value-section_id={@section_view.id}
          >
            <.remix_icon icon="close-line" />
            <span>Clear</span>
          </button>
        </.menu_item>
        <div class="my-1 border-t border-gray-200"></div>
      <% end %>
      <.menu_item :for={parent <- @section_view.valid_parents}>
        <button
          class="text-gray-500"
          phx-click="set_section_parent"
          phx-value-section_id={@section_view.id}
          phx-value-parent_id={parent.id}
        >
          <.remix_icon
            :if={@section_view.parent}
            icon="arrow-right-s-line"
            class={[(@section_view.parent && @section_view.parent.id == parent.id) || "invisible"]}
          />
          <span>{parent.name}</span>
        </button>
      </.menu_item>
    </.menu>
    """
  end

  defp cannot_branch_out_reason(%{valid_parents: []}),
    do: "No section to branch out from"

  defp cannot_branch_out_reason(%{has_children?: true}),
    do: "Cannot branch out this section because\nother sections branch from it"

  defp cannot_branch_out_reason(_section_view), do: nil
end

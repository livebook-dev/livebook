defmodule LivebookWeb.SessionLive.IndicatorsComponent do
  use LivebookWeb, :html

  def render(assigns) do
    ~H"""
    <div class="flex items-center justify-between sticky px-2 top-0 left-0 right-0 z-[500] bg-white border-b border-gray-200">
      <div class="sm:hidden text-2xl text-gray-400 hover:text-gray-600 focus:text-gray-600 rounded-xl h-10 w-10 flex items-center justify-center">
        <button
          aria-label="hide sidebar"
          data-el-toggle-sidebar
          phx-click={
            JS.add_class("hidden sm:flex", to: "[data-el-sidebar]")
            |> JS.toggle(to: "[data-el-toggle-sidebar]", display: "flex")
          }
        >
          <.remix_icon icon="menu-fold-line" />
        </button>

        <button
          class="hidden"
          aria-label="show sidebar"
          data-el-toggle-sidebar
          phx-click={
            JS.remove_class("hidden sm:flex", to: "[data-el-sidebar]")
            |> JS.toggle(to: "[data-el-toggle-sidebar]", display: "flex")
          }
        >
          <.remix_icon icon="menu-unfold-line" />
        </button>
      </div>
      <div class="sm:fixed bottom-[0.4rem] right-[1.5rem]">
        <div
          class="flex flex-row-reverse sm:flex-col items-center justify-end p-2 sm:p-0 space-x-2 space-x-reverse sm:space-x-0 sm:space-y-2"
          data-el-notebook-indicators
        >
          <.view_indicator />
          <.persistence_indicator
            file={@file}
            dirty={@dirty}
            persistence_warnings={@persistence_warnings}
            autosave_interval_s={@autosave_interval_s}
            session_id={@session_id}
          />
          <.runtime_indicator
            runtime={@runtime}
            global_status={@global_status}
            session_id={@session_id}
          />
          <.insert_mode_indicator />
        </div>
      </div>
    </div>
    """
  end

  defp view_indicator(assigns) do
    ~H"""
    <div class="tooltip left" data-tooltip="Choose views to activate" data-el-views>
      <.menu id="views-menu" position={:bottom_right} sm_position={:top_right}>
        <:toggle>
          <button
            class="icon-button icon-outlined-button border-gray-200 hover:bg-gray-100 focus:bg-gray-100"
            aria-label="choose views to activate"
            data-el-views-disabled
          >
            <.remix_icon icon="layout-5-line" class="text-xl text-gray-400" />
          </button>
          <button
            class="icon-button icon-outlined-button border-green-bright-300 hover:bg-green-bright-50 focus:bg-green-bright-50"
            aria-label="choose views to activate"
            data-el-views-enabled
          >
            <.remix_icon icon="layout-5-line" class="text-xl text-green-bright-400" />
          </button>
        </:toggle>
        <.menu_item>
          <button role="menuitem" data-el-view-toggle="code-zen">
            <.remix_icon icon="code-line" />
            <span>Code zen</span>
          </button>
        </.menu_item>
        <.menu_item>
          <button role="menuitem" data-el-view-toggle="presentation">
            <.remix_icon icon="slideshow-2-line" />
            <span>Presentation</span>
          </button>
        </.menu_item>
        <.menu_item>
          <button role="menuitem" data-el-view-toggle="custom">
            <.remix_icon icon="settings-5-line" />
            <span>Custom</span>
          </button>
        </.menu_item>
      </.menu>
    </div>
    """
  end

  defp persistence_indicator(%{file: nil} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Choose a file to save the notebook">
      <.link
        patch={~p"/sessions/#{@session_id}/settings/file"}
        class="icon-button icon-outlined-button border-gray-200 hover:bg-gray-100 focus:bg-gray-100"
        aria-label="choose a file to save the notebook"
      >
        <.remix_icon icon="save-line" class="text-xl text-gray-400" />
      </.link>
    </span>
    """
  end

  defp persistence_indicator(%{dirty: false} = assigns) do
    ~H"""
    <span
      class="tooltip left"
      data-tooltip={
        case @persistence_warnings do
          [] ->
            "Notebook saved"

          warnings ->
            "Notebook saved with warnings:\n" <> Enum.map_join(warnings, "\n", &("- " <> &1))
        end
      }
    >
      <.link
        patch={~p"/sessions/#{@session_id}/settings/file"}
        class="icon-button icon-outlined-button border-green-bright-300 hover:bg-green-bright-50 focus:bg-green-bright-50 relative"
        aria-label="notebook saved, click to open file settings"
      >
        <.remix_icon icon="save-line" class="text-xl text-green-bright-400" />
        <.remix_icon
          :if={@persistence_warnings != []}
          icon="error-warning-fill"
          class="text-lg text-red-400 absolute -top-1.5 -right-2"
        />
      </.link>
    </span>
    """
  end

  defp persistence_indicator(%{autosave_interval_s: nil} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="No autosave configured, make sure to save manually">
      <.link
        patch={~p"/sessions/#{@session_id}/settings/file"}
        class="icon-button icon-outlined-button border-yellow-bright-200 hover:bg-red-50 focus:bg-red-50"
        aria-label="no autosave configured, click to open file settings"
      >
        <.remix_icon icon="save-line" class="text-xl text-yellow-bright-300" />
      </.link>
    </span>
    """
  end

  defp persistence_indicator(assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Autosave pending">
      <.link
        patch={~p"/sessions/#{@session_id}/settings/file"}
        class="icon-button icon-outlined-button border-blue-400 hover:bg-blue-50 focus:bg-blue-50"
        aria-label="autosave pending, click to open file settings"
      >
        <.remix_icon icon="save-line" class="text-xl text-blue-500" />
      </.link>
    </span>
    """
  end

  defp runtime_indicator(assigns) do
    ~H"""
    <%= if Livebook.Runtime.connected?(@runtime) do %>
      <.global_status status={elem(@global_status, 0)} cell_id={elem(@global_status, 1)} />
    <% else %>
      <span class="tooltip left" data-tooltip="Choose a runtime to run the notebook in">
        <.link
          patch={~p"/sessions/#{@session_id}/settings/runtime"}
          class="icon-button icon-outlined-button border-gray-200 hover:bg-gray-100 focus:bg-gray-100"
          aria-label="choose a runtime to run the notebook in"
        >
          <.remix_icon icon="loader-3-line" class="text-xl text-gray-400" />
        </.link>
      </span>
    <% end %>
    """
  end

  defp global_status(%{status: :evaluating} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Go to evaluating cell">
      <button
        class="border-blue-400 icon-button icon-outlined-button hover:bg-blue-50 focus:bg-blue-50"
        aria-label="go to evaluating cell"
        data-el-focus-cell-button
        data-target={@cell_id}
      >
        <.remix_icon icon="loader-3-line" class="text-xl text-blue-500 animate-spin" />
      </button>
    </span>
    """
  end

  defp global_status(%{status: :evaluated} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Go to last evaluated cell">
      <button
        class="border-green-bright-300 icon-button icon-outlined-button hover:bg-green-bright-50 focus:bg-green-bright-50"
        aria-label="go to last evaluated cell"
        data-el-focus-cell-button
        data-target={@cell_id}
      >
        <.remix_icon icon="loader-3-line" class="text-xl text-green-bright-400" />
      </button>
    </span>
    """
  end

  defp global_status(%{status: :errored} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Go to last evaluated cell">
      <button
        class="border-red-300 icon-button icon-outlined-button hover:bg-red-50 focus:bg-red-50"
        aria-label="go to last evaluated cell"
        data-el-focus-cell-button
        data-target={@cell_id}
      >
        <.remix_icon icon="loader-3-line" class="text-xl text-red-400" />
      </button>
    </span>
    """
  end

  defp global_status(%{status: :stale} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Go to first stale cell">
      <button
        class="border-yellow-bright-200 icon-button icon-outlined-button hover:bg-yellow-bright-50 focus:bg-yellow-bright-50"
        aria-label="go to first stale cell"
        data-el-focus-cell-button
        data-target={@cell_id}
      >
        <.remix_icon icon="loader-3-line" class="text-xl text-yellow-bright-300" />
      </button>
    </span>
    """
  end

  defp global_status(%{status: :fresh} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Ready to evaluate">
      <div
        class="border-gray-200 icon-button icon-outlined-button hover:bg-gray-100 focus:bg-gray-100"
        aria-label="ready to evaluate"
      >
        <.remix_icon icon="loader-3-line" class="text-xl text-gray-400" />
      </div>
    </span>
    """
  end

  defp insert_mode_indicator(assigns) do
    ~H"""
    <% # Note: this indicator is shown/hidden using CSS based on the current mode %>
    <span class="tooltip left" data-tooltip="Insert mode" data-el-insert-mode-indicator>
      <span class="text-sm font-medium text-gray-400 cursor-default">
        ins
      </span>
    </span>
    """
  end
end

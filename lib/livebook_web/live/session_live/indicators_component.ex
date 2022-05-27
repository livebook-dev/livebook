defmodule LivebookWeb.SessionLive.IndicatorsComponent do
  use Phoenix.Component

  alias Phoenix.LiveView.JS
  alias LivebookWeb.Router.Helpers, as: Routes

  import LivebookWeb.LiveHelpers

  def render(assigns) do
    ~H"""
    <div class="flex items-center justify-between sticky px-2 top-0 left-0 right-0 z-[500] bg-white border-b border-gray-200">
      <div class="sm:hidden text-2xl text-gray-400 hover:text-gray-600 focus:text-gray-600 rounded-xl h-10 w-10 flex items-center justify-center">
        <button
          aria-label="hide sidebar"
          data-el-toggle-sidebar
          phx-click={JS.add_class("hidden sm:flex", to: "[data-el-sidebar]") |> JS.toggle(to: "[data-el-toggle-sidebar]", display: "flex")}>
            <.remix_icon icon="menu-fold-line" />
        </button>

        <button
          class="hidden"
          aria-label="show sidebar"
          data-el-toggle-sidebar
          phx-click={JS.remove_class("hidden sm:flex", to: "[data-el-sidebar]") |> JS.toggle(to: "[data-el-toggle-sidebar]", display: "flex")}>
            <.remix_icon icon="menu-unfold-line" />
        </button>
      </div>
      <div class="sm:fixed bottom-[0.4rem] right-[1.5rem]">
        <div class="flex flex-row-reverse sm:flex-col items-center justify-end p-2 sm:p-0 space-x-2 space-x-reverse sm:space-x-0 sm:space-y-2" data-el-notebook-indicators>
          <.code_zen_indicator />
          <.persistence_indicator
            file={@file}
            dirty={@dirty}
            autosave_interval_s={@autosave_interval_s}
            socket={@socket}
            session_id={@session_id} />
          <.runtime_indicator
            runtime={@runtime}
            global_status={@global_status}
            socket={@socket}
            session_id={@session_id} />
          <.insert_mode_indicator />
        </div>
      </div>
    </div>
    """
  end

  defp code_zen_indicator(assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Enter code zen (z)" data-el-code-zen-enable>
      <button class="icon-button icon-outlined-button border-gray-200 hover:bg-gray-100 focus:bg-gray-100"
        aria-label="enter code zen"
        data-el-code-zen-enable-button>
        <.remix_icon icon="code-line" class="text-xl text-gray-400" />
      </button>
    </span>
    <div data-el-focus-mode-options>
      <.menu id="focus-mode-menu" position="top-right">
        <:toggle>
          <button class="icon-button icon-outlined-button border-green-bright-300 hover:bg-green-bright-50 focus:bg-green-bright-50"
            aria-label="code zen options">
            <.remix_icon icon="code-line" class="text-xl text-green-bright-400" />
          </button>
        </:toggle>
        <:content>
          <button class="menu-item text-gray-500"
            role="menuitem"
            data-el-code-zen-outputs-toggle>
            <.remix_icon icon="layout-bottom-2-line" />
            <span class="font-medium" data-label-show>Show outputs</span>
            <span class="font-medium" data-label-hide>Hide outputs</span>
          </button>
          <button class="menu-item text-gray-500"
            role="menuitem"
            data-el-code-zen-disable-button>
            <.remix_icon icon="close-line" />
            <span class="font-medium">Exit code zen</span>
          </button>
        </:content>
      </.menu>
    </div>
    """
  end

  defp persistence_indicator(%{file: nil} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Choose a file to save the notebook">
      <%= live_patch to: Routes.session_path(@socket, :file_settings, @session_id),
            class: "icon-button icon-outlined-button border-gray-200 hover:bg-gray-100 focus:bg-gray-100",
            aria_label: "choose a file to save the notebook" do %>
        <.remix_icon icon="save-line" class="text-xl text-gray-400" />
      <% end %>
    </span>
    """
  end

  defp persistence_indicator(%{dirty: false} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Notebook saved">
      <%= live_patch to: Routes.session_path(@socket, :file_settings, @session_id),
            class: "icon-button icon-outlined-button border-green-bright-300 hover:bg-green-bright-50 focus:bg-green-bright-50",
            aria_label: "notebook saved, click to open file settings" do %>
        <.remix_icon icon="save-line" class="text-xl text-green-bright-400" />
      <% end %>
    </span>
    """
  end

  defp persistence_indicator(%{autosave_interval_s: nil} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="No autosave configured, make sure to save manually">
      <%= live_patch to: Routes.session_path(@socket, :file_settings, @session_id),
            class: "icon-button icon-outlined-button border-yellow-bright-200 hover:bg-red-50 focus:bg-red-50",
            aria_label: "no autosave configured, click to open file settings" do %>
        <.remix_icon icon="save-line" class="text-xl text-yellow-bright-300" />
      <% end %>
    </span>
    """
  end

  defp persistence_indicator(assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Autosave pending">
      <%= live_patch to: Routes.session_path(@socket, :file_settings, @session_id),
            class: "icon-button icon-outlined-button border-blue-400 hover:bg-blue-50 focus:bg-blue-50",
            aria_label: "autosave pending, click to open file settings" do %>
        <.remix_icon icon="save-line" class="text-xl text-blue-500" />
      <% end %>
    </span>
    """
  end

  defp runtime_indicator(assigns) do
    ~H"""
    <%= if Livebook.Runtime.connected?(@runtime) do %>
      <.global_status
        status={elem(@global_status, 0)}
        cell_id={elem(@global_status, 1)} />
    <% else %>
      <span class="tooltip left" data-tooltip="Choose a runtime to run the notebook in">
        <%= live_patch to: Routes.session_path(@socket, :runtime_settings, @session_id),
              class: "icon-button icon-outlined-button border-gray-200 hover:bg-gray-100 focus:bg-gray-100",
              aria_label: "choose a runtime to run the notebook in" do %>
          <.remix_icon icon="loader-3-line" class="text-xl text-gray-400" />
        <% end %>
      </span>
    <% end %>
    """
  end

  defp global_status(%{status: :evaluating} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Go to evaluating cell">
      <button class="border-blue-400 icon-button icon-outlined-button hover:bg-blue-50 focus:bg-blue-50"
        aria-label="go to evaluating cell"
        data-el-focus-cell-button
        data-target={@cell_id}>
        <.remix_icon icon="loader-3-line" class="text-xl text-blue-500 animate-spin" />
      </button>
    </span>
    """
  end

  defp global_status(%{status: :evaluated} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Go to last evaluated cell">
      <button class="border-green-bright-300 icon-button icon-outlined-button hover:bg-green-bright-50 focus:bg-green-bright-50"
        aria-label="go to last evaluated cell"
        data-el-focus-cell-button
        data-target={@cell_id}>
        <.remix_icon icon="loader-3-line" class="text-xl text-green-bright-400" />
      </button>
    </span>
    """
  end

  defp global_status(%{status: :stale} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Go to first stale cell">
      <button class="border-yellow-bright-200 icon-button icon-outlined-button hover:bg-yellow-bright-50 focus:bg-yellow-bright-50"
        aria-label="go to first stale cell"
        data-el-focus-cell-button
        data-target={@cell_id}>
        <.remix_icon icon="loader-3-line" class="text-xl text-yellow-bright-300" />
      </button>
    </span>
    """
  end

  defp global_status(%{status: :fresh} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Ready to evaluate">
      <div class="border-gray-200 icon-button icon-outlined-button hover:bg-gray-100 focus:bg-gray-100"
        aria-label="ready to evaluate">
        <.remix_icon icon="loader-3-line" class="text-xl text-gray-400" />
      </div>
    </span>
    """
  end

  defp insert_mode_indicator(assigns) do
    ~H"""
    <%# Note: this indicator is shown/hidden using CSS based on the current mode %>
    <span class="tooltip left" data-tooltip="Insert mode" data-el-insert-mode-indicator>
      <span class="text-sm font-medium text-gray-400 cursor-default">
        ins
      </span>
    </span>
    """
  end
end

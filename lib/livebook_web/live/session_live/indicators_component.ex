defmodule LivebookWeb.SessionLive.IndicatorsComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col items-center space-y-2" data-el-notebook-indicators>
      <.code_zen_indicator />
      <%= if @file do %>
        <%= if @dirty do %>
          <%= if @autosave_interval_s do %>
            <span class="tooltip left" data-tooltip="Autosave pending">
              <%= live_patch to: Routes.session_path(@socket, :file_settings, @session_id),
                    class: "icon-button icon-outlined-button border-blue-400 hover:bg-blue-50 focus:bg-blue-50",
                    aria_label: "autosave pending, click to open file settings" do %>
                <.remix_icon icon="save-line" class="text-xl text-blue-500" />
              <% end %>
            </span>
          <% else %>
            <span class="tooltip left" data-tooltip="No autosave configured, make sure to save manually">
              <%= live_patch to: Routes.session_path(@socket, :file_settings, @session_id),
                    class: "icon-button icon-outlined-button border-yellow-bright-200 hover:bg-red-50 focus:bg-red-50",
                    aria_label: "no autosave configured, click to open file settings" do %>
                <.remix_icon icon="save-line" class="text-xl text-yellow-bright-300" />
              <% end %>
            </span>
          <% end %>
        <% else %>
          <span class="tooltip left" data-tooltip="Notebook saved">
            <%= live_patch to: Routes.session_path(@socket, :file_settings, @session_id),
                  class: "icon-button icon-outlined-button border-green-bright-300 hover:bg-green-bright-50 focus:bg-green-bright-50",
                  aria_label: "notebook saved, click to open file settings" do %>
              <.remix_icon icon="save-line" class="text-xl text-green-bright-400" />
            <% end %>
          </span>
        <% end %>
      <% else %>
        <span class="tooltip left" data-tooltip="Choose a file to save the notebook">
          <%= live_patch to: Routes.session_path(@socket, :file_settings, @session_id),
                class: "icon-button icon-outlined-button border-gray-200 hover:bg-gray-100 focus:bg-gray-100",
                aria_label: "choose a file to save the notebook" do %>
            <.remix_icon icon="save-line" class="text-xl text-gray-400" />
          <% end %>
        </span>
      <% end %>

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

      <%# Note: this indicator is shown/hidden using CSS based on the current mode %>
      <span class="tooltip left" data-tooltip="Insert mode" data-el-insert-mode-indicator>
        <span class="text-sm font-medium text-gray-400 cursor-default">
          ins
        </span>
      </span>
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
end

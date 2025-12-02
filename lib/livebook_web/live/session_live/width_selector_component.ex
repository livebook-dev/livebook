defmodule LivebookWeb.SessionLive.WidthSelectorComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.menu id="width-selector-menu" position="bottom-left">
        <:toggle>
          <span class="tooltip left" data-tooltip="Set notebook container width">
            <button
              class="inline-flex items-center gap-1 px-2 py-1 mt-0.5 text-sm text-gray-600 hover:text-gray-800 hover:bg-gray-50 rounded-lg transition-colors"
              aria-label="Container width"
            >
              <.remix_icon icon="layout-column-line" class="text-base" />
              <span>{width_label(@container_width)}</span>
              <.remix_icon icon="arrow-down-s-line" class="text-sm -ml-0.5" />
            </button>
          </span>
        </:toggle>
        <.menu_item>
          <button
            phx-click="set_container_width"
            phx-value-width="default"
            phx-target={@myself}
            role="menuitem"
            class="w-full"
          >
            <div class="flex items-center justify-between">
              <span>Regular</span>
              <.remix_icon
                :if={@container_width == :default}
                icon="check-line"
                class="text-blue-600"
              />
            </div>
          </button>
        </.menu_item>
        <.menu_item>
          <button
            phx-click="set_container_width"
            phx-value-width="wide"
            phx-target={@myself}
            role="menuitem"
            class="w-full"
          >
            <div class="flex items-center justify-between">
              <span>Wide</span>
              <.remix_icon
                :if={@container_width == :wide}
                icon="check-line"
                class="text-blue-600"
              />
            </div>
          </button>
        </.menu_item>
        <.menu_item>
          <button
            phx-click="set_container_width"
            phx-value-width="full"
            phx-target={@myself}
            role="menuitem"
            class="w-full"
          >
            <div class="flex items-center justify-between">
              <span>Full</span>
              <.remix_icon :if={@container_width == :full} icon="check-line" class="text-blue-600" />
            </div>
          </button>
        </.menu_item>
      </.menu>
    </div>
    """
  end

  @impl true
  def handle_event("set_container_width", %{"width" => width}, socket) do
    container_width = parse_width(width)
    send(self(), {:set_notebook_attributes, %{container_width: container_width}})
    {:noreply, socket}
  end

  defp width_label(:default), do: "Regular width"
  defp width_label(:wide), do: "Wide width"
  defp width_label(:full), do: "Full-width"
  defp width_label(_), do: "Regular"

  defp parse_width("default"), do: :default
  defp parse_width("wide"), do: :wide
  defp parse_width("full"), do: :full
  defp parse_width(_), do: :default
end

defmodule LivebookWeb.GridstackComponent do
  use LivebookWeb, :live_component

  # The component expects:
  #
  #   * `:grid_id` - id of the grid
  #   * `:columns` - the number of columns for the grid

  @impl true
  def update(assigns, socket) do
    socket =
      socket
      |> assign(
        id: assigns.id,
        columns: assigns.columns,
        cell_height: assigns.columns || "auto"
      )
      |> stream(:grid_components, assigns.grid_components)
    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="grid-stack" gs-column={@columns} gs-cell-height={@cell_height} phx-hook="Gridstack" data-phx-target={@myself}>
      <div
        :for={{_dom_id, item} <- @streams.grid_components}
        class="grid-stack-item"
        gs-id={item.id}
        gs-x={item.x_pos}
        gs-y={item.y_pos}
        gs-w={item.width}
        gs-h={item.height}
      >
        <div class="grid-stack-item-content"><%= item.content %></div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("items_changed", params, socket) do
    params =
      for {key, %{"x_pos" => x_pos, "y_pos" => y_pos, "width" => width, "height" => height}} <-
            params,
          into: %{} do
        {key, %{x_pos: x_pos, y_pos: y_pos, width: width, height: height}}
      end

    IO.inspect(params, label: "params")
    {:noreply, socket}
  end
end

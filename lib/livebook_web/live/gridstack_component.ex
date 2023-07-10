defmodule LivebookWeb.GridstackComponent do
  use LivebookWeb, :live_component

  # The component expects:
  #
  #   * `:output_blocks` - TODO

  @impl true
  def update(assigns, socket) do
    socket =
      socket
      |> assign(output_blocks: assigns.output_blocks)
    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id="dashboard_container" class="w-screen h-screen"  phx-update="ignore">
      <div
        id="gridstack_container"
        class="grid-stack"
        gs-column="12"
        phx-hook="Gridstack"
      >
        <div
          :for={output_block <- @output_blocks}
          class="relative grid-stack-item rounded border-2 border-red-500"
          gs-id={output_block.id}
          gs-x={output_block.x_pos}
          gs-y={output_block.y_pos}
          gs-w={output_block.width}
          gs-h={output_block.height}
        >
          <div class="absolute grid-stack-item-content">
            <div class="absolute inset-0 bg-transparent z-20 hover:cursor-move" />
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("items_changed", params, socket) do
    IO.inspect(params, label: "params")

    params =
      for {key, %{"x_pos" => x_pos, "y_pos" => y_pos, "width" => width, "height" => height}} <-
            params,
          into: %{} do
        {key, %{x_pos: x_pos, y_pos: y_pos, width: width, height: height}}
      end
    {:noreply, socket}
  end

  @impl true
  def handle_event("insert", %{"id" => id, "x" => x, "y" => y, "w" => w, "h" => h} = params, socket) do
    IO.inspect(params, label: "INSERT")
    IO.inspect("HELLO", label: "INSERT")
    {:noreply, stream_insert(socket, :grid_components, %{id: id, x_pos: x, y_pos: y, width: w, height: h}, at: 0)}
  end

  @impl true
  def handle_event("delete", %{"id" => dom_id} = params , socket) do
    IO.inspect("HELLO", label: "DELELTE")
    IO.inspect(params, label: "DELELTE")
    {:noreply, stream_delete_by_dom_id(socket, :grid_components, dom_id)}
  end
end

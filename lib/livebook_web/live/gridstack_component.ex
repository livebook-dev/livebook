defmodule LivebookWeb.GridstackComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  # The component expects:
  #
  #   * `:output_blocks` - TODO

  @impl true
  def update(assigns, socket) do
    IO.inspect(assigns, label: "UPDATED")
    socket =
      socket
      |> assign(
        output_blocks: assigns.output_blocks,
        session: assigns.session,
        app_settings: assigns.app_settings
      )
    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id="gridstack-container" phx-update="ignore">
      <div
        id="gridstack-grid"
        class="grid-stack"
        gs-column="12"
        phx-hook="Gridstack"
        data-phx-target={@myself}
      >
        <div
          :for={output_block <- @output_blocks}
          class="relative grid-stack-item rounded border-2 border-red-500"
          gs-id={output_block.id}
          gs-x={output_block.x}
          gs-y={output_block.y}
          gs-w={output_block.w}
          gs-h={output_block.h}
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
    {:noreply, assign(socket, output_blocks: params)}
  end

  def handle_event("new_app_layout", %{"layout" => layout} = data, socket) do
    # TODO: tidy up
    IO.inspect(data)
    socket = put_in(socket.assigns.app_settings.output_layout, layout)

    Session.set_app_settings(
      socket.assigns.session.pid,
      socket.assigns.app_settings
    )

    {:noreply, socket}
  end

end

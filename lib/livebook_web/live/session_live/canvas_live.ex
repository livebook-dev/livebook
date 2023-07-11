defmodule LivebookWeb.SessionLive.CanvasLive do
  use LivebookWeb, :live_view

  @impl true
  def mount(
        _params,
        %{
          "session" => session,
          "runtime" => runtime,
          "client_id" => client_id,
          "canvas_settings" => canvas_settings
        },
        socket
      ) do
    socket =
      socket
      |> assign(
        session: session,
        runtime: runtime,
        client_id: client_id,
        canvas_settings: canvas_settings
      )

    if connected?(socket) do
      send(socket.parent_pid, {:canvas_pid, self()})
    end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id="gridstack-container">
      <div data-el-js-view-iframes phx-update="ignore" id="js-view-iframes-canvas"></div>
      <div id="canvas-grid" class="grid-stack" gs-column="12" phx-hook="Gridstack">
        <div
          :for={{cell_id, item} <- @canvas_settings.items}
          class="grid-stack-item rounded border-2 border-red-500"
          gs-id={"canvas-#{cell_id}"}
          gs-x={item[:x]}
          gs-y={item[:y]}
          gs-w={item[:w]}
          gs-h={item[:h]}
        >
          <div class="grid-stack-content">
            <div
              class="flex flex-col"
              data-el-outputs-container
              id={"outputs-#{cell_id}"}
              phx-update="append"
            >
              <LivebookWeb.Output.outputs
                outputs={item.outputs}
                dom_id_map={%{}}
                session_id={@session.id}
                session_pid={@session.pid}
                client_id={@client_id}
                cell_id={cell_id}
                output_location={:canvas}
                input_values={%{}}
              />
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("items_changed", params, socket) do
    # Session.update_canvas_layout(socket.assigns.session.pid, app_settings)

    {:noreply, socket}
  end

  @impl true
  def handle_info({:new_data, canvas_settings}, socket) do
    {:noreply, assign(socket, canvas_settings: canvas_settings)}
  end
end

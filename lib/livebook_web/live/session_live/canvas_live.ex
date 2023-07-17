defmodule LivebookWeb.SessionLive.CanvasLive do
  use LivebookWeb, :live_view

  alias Livebook.{Session, Sessions}

  @impl true
  def mount(
        _params,
        %{
          "session" => session,
          "client_id" => client_id,
          "canvas_settings" => canvas_settings
        },
        socket
      ) do
    socket =
      socket
      |> assign(
        session: session,
        client_id: client_id,
        canvas_settings: canvas_settings,
        extern_window: false
      )

    if connected?(socket) do
      send(socket.parent_pid, {:canvas_pid, self()})
    end

    {:ok, socket}
  end

  def mount(%{"id" => session_id}, _session, socket) do
    {:ok, session} = Sessions.fetch_session(session_id)

    {data, client_id} =
      Session.register_client(session.pid, self(), socket.assigns.current_user)

    socket =
      socket
      |> assign(
        session: session,
        client_id: client_id,
        canvas_settings: data.notebook.canvas_settings,
        extern_window: true
      )

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id="gridstack-container">
      <div data-el-js-view-iframes phx-update="ignore" id="js-view-iframes-canvas"></div>
      <.canvas_settings_button extern_window={@extern_window} />
      <div
        id="canvas-grid"
        class="grid-stack"
        gs-column="12"
        phx-hook="Gridstack"
        data-el-js-extern-window={@extern_window}
      >
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

  defp canvas_settings_button(assigns) do
    ~H"""
    <div class="fixed top-[1rem] right-[1.5rem]">
      <div class="tooltip left" data-tooltip="Canvas Options" data-el-canvas-menu>
        <.menu id="canvas-menu" position={:bottom_right}>
          <:toggle>
            <button
              class="icon-button icon-outlined-button border-gray-200 hover:bg-gray-100 focus:bg-gray-100"
              aria-label="canvas options"
            >
              <.remix_icon icon="more-2-fill" class="text-xl text-gray-400" />
            </button>
          </:toggle>
          <.menu_item :if={not @extern_window}>
            <button role="menuitem" data-el-canvas-popout-button>
              <.remix_icon icon="external-link-line" />
              <span>Pop-Out</span>
            </button>
          </.menu_item>
          <.menu_item :if={@extern_window}>
            <button role="menuitem" data-el-canvas-popin-button>
              <.remix_icon icon="corner-left-down-fill" />
              <span>Pop-In</span>
            </button>
          </.menu_item>
          <.menu_item>
            <button role="menuitem" data-el-canvas-close-button>
              <.remix_icon icon="close-fill" />
              <span>Close</span>
            </button>
          </.menu_item>
        </.menu>
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

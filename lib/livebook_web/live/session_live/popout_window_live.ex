defmodule LivebookWeb.SessionLive.PopoutWindowLive do
  use LivebookWeb, :live_view

  alias Livebook.{Session, Sessions}

  @impl true
  def mount(%{"id" => session_id}, _session, socket) do
    {:ok, session} = Sessions.fetch_session(session_id)

    data = Session.get_data(session.pid)

    {:ok,
     socket
     |> assign(
       session: session,
       # TODO
       client_id: "",
       data_view: data_to_view(data)
     )
     |> assign_private(data: data)}
  end

  defp assign_private(socket, assigns) do
    Enum.reduce(assigns, socket, fn {key, value}, socket ->
      put_in(socket.private[key], value)
    end)
  end

  defp data_to_view(data) do
    %{
      canvas_layout: canvas_outputs(data.notebook)
    }
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id="popout-window" class="w-full h-full" phx-hook="PopoutWindow">
      <.live_component
        module={LivebookWeb.SessionLive.CanvasComponent}
        id="canvas"
        canvas_layout={@data_view.canvas_layout}
        session={@session}
        client_id={@client_id}
      />
      <div class="fixed top-[1rem] right-[1.5rem] z-[500]">
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
            <.menu_item>
              <button role="menuitem" data-el-canvas-popin-button>
                <.remix_icon icon="external-link-line" />
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
    </div>
    """
  end
end

defmodule LivebookWeb.SessionLive.PopoutWindowLive do
  use LivebookWeb, :live_view

  alias Livebook.{Notebook, Session, Sessions}
  alias Livebook.Notebook.Cell

  @impl true
  def mount(%{"id" => session_id}, _session, socket) do
    {:ok, session} = Sessions.fetch_session(session_id)

    data = Session.get_data(session.pid)

    Session.subscribe(session_id)

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
      canvas_layout: canvas_outputs(data.notebook),
      output_views:
        for {cell, _section} <- Notebook.cells_with_section(data.notebook),
            Cell.evaluable?(cell),
            cell.id != "setup" do
          %{
            outputs: cell.outputs,
            # input_values: input_values_for_output(cell.outputs, data),
            input_values: %{},
            cell_id: cell.id
          }
        end
    }
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id="popout-window" class="w-full h-full" phx-hook="PopoutWindow">
      <div class="h-full w-full" data-el-notebook>
        <div class="h-full w-full" data-el-notebook-content>
          <div data-el-js-view-iframes phx-update="ignore" id="js-view-iframes"></div>
          <div
            :for={output_view <- Enum.reverse(@data_view.output_views)}
            id={"outputs-#{output_view.cell_id}"}
          >
            <LivebookWeb.Output.outputs
              outputs={output_view.outputs}
              dom_id_map={%{}}
              session_id={@session.id}
              session_pid={@session.pid}
              client_id={@client_id}
              cell_id={output_view.cell_id}
              input_values={output_view.input_values}
            />
          </div>
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
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_info({:operation, operation}, socket) do
    {:noreply, handle_operation(socket, operation)}
  end

  @impl true
  def handle_info(message, socket) do
    IO.inspect(message, label: "Not implemented for Popout Window")
    {:noreply, socket}
  end

  defp handle_operation(socket, operation) do
    case Session.Data.apply_operation(socket.private.data, operation) do
      {:ok, data, _actions} ->
        socket
        |> assign_private(data: data)
        |> assign(
          data_view:
            update_data_view(data, operation)
        )

      :error ->
        socket
    end
  end

  defp update_data_view(data, operation) do
    case operation do
      _ ->
        data_to_view(data)
    end
  end
end

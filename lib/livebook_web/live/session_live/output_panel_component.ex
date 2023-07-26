defmodule LivebookWeb.SessionLive.OutputPanelComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  @impl true
  def mount(socket) do
    {:ok, socket}
  end

  @impl true
  def update(assigns, socket) do
    socket =
      socket
      |> assign(
        session: assigns.session,
        client_id: assigns.client_id,
        output_view: assigns.output_view
      )

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id="output-panel"
      class="grid grid-cols-12"
      phx-hook="OutputPanel"
      data-phx-target={@myself}
      data-el-output-panel-content
    >
      <.dropzone row={0} />
      <%= for {output_row, row_index} <- Enum.with_index(@output_view.rows) do %>
        <div
          :for={item <- output_row.items}
          class="grid col-span-12"
          data-row-index={row_index}
          data-cell-id={item.cell_id}
          draggable="true"
        >
          <LivebookWeb.Output.outputs
            outputs={item.outputs}
            dom_id_map={%{}}
            session_id={@session.id}
            session_pid={@session.pid}
            client_id={@client_id}
            cell_id={item.cell_id}
            input_views={item.input_views}
          />
        </div>
        <.dropzone row={row_index + 1} />
      <% end %>
    </div>
    """
  end

  defp dropzone(assigns) do
    ~H"""
    <div
      class="col-span-12 h-4 bg-white rounded-lg border-2 border-dashed border-gray-400"
      data-el-output-panel-row-drop-area
      data-drop-area-row-index={@row}
      id={"dropzone-row-#{@row}"}
      phx-hook="Dropzone"
    />
    """
  end

  @impl true
  def handle_event(
        "handle_move_item_to_new_row",
        %{"cell_id" => cell_id, "row_num" => row_num},
        socket
      ) do
    Session.move_output_to_new_row(socket.assigns.session.pid, cell_id, row_num)
    {:noreply, socket}
  end
end

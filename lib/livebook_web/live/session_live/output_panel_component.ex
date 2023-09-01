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
        output_views: assigns.output_views
      )

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id="output-panel"
      class="h-full w-full"
      phx-hook="OutputPanel"
      data-phx-target={@myself}
      data-el-output-panel-content
    >
      <h1 class="p-4 text-3xl text-center font-semibold text-gray-800">
        Output Panel
      </h1>
      <.row_dropzone row={0} />
      <%= for {output_row, row_index} <- Enum.with_index(@output_views.rows) do %>
        <div class="flex relative space-y-2" data-row-index={row_index} data-el-output-panel-row>
          <div class="absolute inset-0 flex gap-2" data-el-output-panel-col-drop-area>
            <.col_dropzone
              :for={col_index <- 0..length(output_row.items)}
              row={row_index}
              col={col_index}
            />
          </div>
          <div
            :for={{item, col_index} <- Enum.with_index(output_row.items)}
            id={"output-panel-item-#{row_index}-#{col_index}"}
            class="relative space-x-2"
            style={"width: #{item.width}%"}
            data-cell-id={item.cell_id}
            data-row-index={row_index}
            data-col-index={col_index}
            data-el-output-panel-item
            phx-hook="OutputPanelItem"
          >
            <% cell_rendered? = item.outputs != [] %>
            <.output_options
              cell_id={item.cell_id}
              cell_rendered={cell_rendered?}
              row={row_index}
              col={col_index}
              myself={@myself}
            />
            <%= if cell_rendered? do %>
              <LivebookWeb.Output.outputs
                outputs={item.outputs}
                dom_id_map={%{}}
                session_id={@session.id}
                session_pid={@session.pid}
                client_id={@client_id}
                cell_id={item.cell_id}
                input_views={item.input_views}
              />
            <% else %>
              <div class="info-box">
                This cell was not yet rendered
              </div>
            <% end %>
          </div>
        </div>
        <.row_dropzone row={row_index + 1} />
      <% end %>
    </div>
    """
  end

  defp col_dropzone(assigns) do
    ~H"""
    <div
      id={"dropzone-row-#{@row}-col-#{@col}"}
      class="flex z-20 w-full h-full justify-center items-center"
      data-el-output-panel-col-drop-area
      data-row-index={@row}
      data-col-index={@col}
      phx-hook="OutputPanelDropzone"
      data-js-droppable
    >
      <div class="p-4 text-justify text-center text-sm text-gray-500 font-medium rounded-lg">
        Add to row
      </div>
    </div>
    """
  end

  defp row_dropzone(assigns) do
    ~H"""
    <div
      id={"dropzone-row-#{@row}"}
      class="flex w-full h-2 justify-center items-center"
      data-el-output-panel-row-drop-area
      data-row-index={@row}
      phx-hook="OutputPanelDropzone"
      data-js-droppable
    >
      <div class="p-4 text-justify text-center text-sm text-gray-500 font-medium rounded-lg">
        Add new row
      </div>
    </div>
    """
  end

  defp output_options(assigns) do
    ~H"""
    <div class="absolute z-10 top-0 right-2 hidden" data-el-output-panel-item-options>
      <div class="flex justify-center items-center shadow-lg border rounded border-gray-300 bg-white">
        <div class="flex pr-2" draggable="true" data-row-index={@row} data-col-index={@col}>
          <div class="cursor-move">
            <.remix_icon icon="draggable" />
          </div>
        </div>
        <div>
          <div class="cursor-pointer">
            <%= if @cell_rendered do %>
              <span class="tooltip top" data-tooltip="Reevaluate cell">
                <button
                  class="icon-button"
                  aria-label="reevaluate cell"
                  phx-click="queue_cell_evaluation"
                  phx-value-cell_id={@cell_id}
                  phx-target={@myself}
                >
                  <.remix_icon icon="memories-line" />
                </button>
              </span>
            <% else %>
              <span class="tooltip top" data-tooltip="Evaluate cell">
                <button
                  class="icon-button"
                  aria-label="evaluate cell"
                  phx-click="queue_cell_evaluation"
                  phx-value-cell_id={@cell_id}
                  phx-target={@myself}
                >
                  <.remix_icon icon="play-circle-line" />
                </button>
              </span>
            <% end %>
          </div>
        </div>
        <div class="flex" data-el-output-panel-item-options-controls>
          <div class="cursor-pointer">
            <span class="tooltip top" data-tooltip="Remove output from Output Panel">
              <button
                class="icon-button"
                aria-label="remove output from output panel"
                phx-click="remove_output_from_output_panel"
                phx-value-cell_id={@cell_id}
                phx-target={@myself}
              >
                <.remix_icon icon="logout-box-line" />
              </button>
            </span>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event(
        "handle_move_item",
        %{"cell_id" => cell_id, "row_index" => row_index, "col_index" => col_index},
        socket
      ) do
    Session.move_output_to_new_location(socket.assigns.session.pid, cell_id, row_index, col_index)
    {:noreply, socket}
  end

  @impl true
  def handle_event(
        "handle_move_item_to_new_row",
        %{"cell_id" => cell_id, "row_index" => row_index},
        socket
      ) do
    Session.move_output_to_new_row(socket.assigns.session.pid, cell_id, row_index)
    {:noreply, socket}
  end

  def handle_event("remove_output_from_output_panel", %{"cell_id" => cell_id}, socket) do
    Session.remove_output_from_output_panel(socket.assigns.session.pid, cell_id)
    {:noreply, socket}
  end

  def handle_event("queue_cell_evaluation", %{"cell_id" => cell_id}, socket) do
    Session.queue_cell_evaluation(socket.assigns.session.pid, cell_id)
    {:noreply, socket}
  end
end

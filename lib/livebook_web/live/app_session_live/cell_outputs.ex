defmodule LivebookWeb.AppSessionLive.CellOutputsComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, stream(socket, :outputs, [])}
  end

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    stream_items =
      for {idx, output} <- Enum.reverse(assigns.cell_view.outputs) do
        %{id: Integer.to_string(idx), idx: idx, output: output}
      end

    socket = stream(socket, :outputs, stream_items)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id={"#{@id}-#{@cell_view.outputs_batch_number}"}
      phx-update="stream"
      class="empty:hidden"
      phx-no-format
    ><LivebookWeb.Output.output
       :for={{dom_id, output} <- @streams.outputs}
       id={dom_id}
       output={output.output}
       session_id={@session.id}
       session_pid={@session.pid}
       client_id={@client_id}
       cell_id={@cell_view.id}
       input_views={@cell_view.input_views}
    /></div>
    """
  end
end

defmodule LivebookWeb.Output.GridComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, stream(socket, :outputs, [])}
  end

  @impl true
  def update(assigns, socket) do
    {outputs, assigns} = Map.pop!(assigns, :outputs)

    socket = assign(socket, assigns)

    stream_items =
      for {idx, output} <- outputs do
        id = "#{idx}-grid-item"
        %{id: id, idx: idx, output: output}
      end

    socket = stream(socket, :outputs, stream_items)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="overflow-auto tiny-scrollbar">
      <div
        id={"#{@id}-grid"}
        class="grid grid-cols-2 w-full"
        style={"grid-template-columns: repeat(#{@columns}, minmax(0, 1fr)); gap: #{@gap}px"}
        phx-update="stream"
      >
        <div :for={{dom_id, output} <- @streams.outputs} id={dom_id}>
          <LivebookWeb.Output.output
            id={"outputs-#{output.idx}"}
            output={output.output}
            session_id={@session_id}
            session_pid={@session_pid}
            input_views={@input_views}
            client_id={@client_id}
            cell_id={@cell_id}
          />
        </div>
      </div>
    </div>
    """
  end
end

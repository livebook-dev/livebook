defmodule LivebookWeb.Output.FrameComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, stream(socket, :outputs, [])}
  end

  @impl true
  def update(%{event: {:update, update_type, outputs, input_views}}, socket) do
    socket = assign(socket, input_views: input_views)

    socket =
      case update_type do
        :replace ->
          socket
          |> assign(num_outputs: length(outputs))
          |> stream(:outputs, stream_items(outputs), reset: true)

        :append ->
          socket
          |> update(:num_outputs, &(length(outputs) + &1))
          |> stream(:outputs, stream_items(outputs))
      end

    {:ok, socket}
  end

  def update(assigns, socket) do
    {outputs, assigns} = Map.pop!(assigns, :outputs)

    socket = assign(socket, assigns)

    socket = assign_new(socket, :num_outputs, fn -> length(outputs) end)
    socket = stream(socket, :outputs, stream_items(outputs))

    {:ok, socket}
  end

  defp stream_items(outputs) do
    for {idx, output} <- Enum.reverse(outputs) do
      %{id: Integer.to_string(idx), idx: idx, output: output}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id}>
      <%= if @num_outputs == 0 do %>
        <div :if={@placeholder} class="text-gray-300 p-4 rounded-lg border border-gray-200">
          Nothing here...
        </div>
      <% else %>
        <div id={"#{@id}-outputs"} phx-update="stream">
          <LivebookWeb.Output.output
            :for={{dom_id, output} <- @streams.outputs}
            id={dom_id}
            output={output.output}
            session_id={@session_id}
            session_pid={@session_pid}
            input_views={@input_views}
            client_id={@client_id}
            cell_id={@cell_id}
          />
        </div>
      <% end %>
    </div>
    """
  end
end

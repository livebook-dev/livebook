defmodule LivebookWeb.Output.FrameComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, counter: 0, output_count: 0, idx_map: %{})}
  end

  @impl true
  def update(assigns, socket) do
    {update_type, assigns} = Map.pop(assigns, :update_type, nil)
    {outputs, assigns} = Map.pop!(assigns, :outputs)

    socket = assign(socket, assigns)

    socket =
      if socket.assigns.counter == 0 do
        assign(socket,
          counter: 1,
          output_count: length(outputs),
          idx_map: append_idx_map(%{}, outputs)
        )
      else
        socket
      end

    socket =
      case update_type do
        nil ->
          assign(socket, outputs: outputs)

        :replace ->
          prev_output_count = socket.assigns.output_count
          output_count = length(outputs)
          idx_map = append_idx_map(%{}, outputs)

          socket = assign(socket, outputs: outputs, output_count: output_count, idx_map: idx_map)

          # If there are less outputs we reset the counter to remove DOM elements
          if output_count < prev_output_count do
            update(socket, :counter, &(&1 + 1))
          else
            socket
          end

        :append ->
          socket
          |> update(:idx_map, &append_idx_map(&1, outputs))
          |> update(:outputs, &(outputs ++ &1))
          |> update(:output_count, &(length(outputs) + &1))
      end

    {:ok, socket}
  end

  defp append_idx_map(idx_map, outputs) do
    next_idx = map_size(idx_map)

    outputs
    |> Enum.with_index(next_idx)
    |> Map.new(fn {{output_idx, _}, idx} -> {output_idx, idx} end)
    |> Map.merge(idx_map)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={"frame-output-#{@id}"}>
      <%= if @output_count == 0 do %>
        <div class="text-gray-300 p-4 rounded-lg border border-gray-200">
          Empty output frame
        </div>
      <% else %>
        <div id={"frame-outputs-#{@id}-#{@counter}"} phx-update="append">
          <LivebookWeb.Output.outputs
            outputs={@outputs}
            dom_id_map={@idx_map}
            socket={@socket}
            session_id={@session_id}
            input_values={@input_values}
            runtime={nil}
            cell_validity_status={nil} />
          </div>
      <% end %>
    </div>
    """
  end
end

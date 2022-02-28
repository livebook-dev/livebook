defmodule LivebookWeb.Output.FrameComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, counter: 0, output_count: 0, persistent_id_map: %{})}
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
          persistent_id_map: map_idx_to_persistent_id(outputs, socket.assigns.id)
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
          prev_persistent_id_map = socket.assigns.persistent_id_map

          output_count = length(outputs)
          persistent_id_map = map_idx_to_persistent_id(outputs, socket.assigns.id)

          socket =
            assign(socket,
              outputs: outputs,
              output_count: output_count,
              persistent_id_map: persistent_id_map
            )

          less_outputs? = prev_output_count > output_count
          appended_outputs? = prev_output_count > map_size(prev_persistent_id_map)

          # If there are outputs that we need to remove, increase the counter.
          # Otherwise we reuse DOM element ids via persistent_id_map
          if less_outputs? or appended_outputs? do
            update(socket, :counter, &(&1 + 1))
          else
            socket
          end

        :append ->
          socket
          |> update(:outputs, &(outputs ++ &1))
          |> update(:output_count, &(length(outputs) + &1))
      end

    {:ok, socket}
  end

  defp map_idx_to_persistent_id(outputs, root_id) do
    outputs
    |> Enum.with_index()
    |> Map.new(fn {{output_idx, _}, idx} -> {output_idx, "#{root_id}-#{idx}"} end)
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
            dom_id_map={@persistent_id_map}
            socket={@socket}
            session_id={@session_id}
            input_values={@input_values}
            runtime={nil}
            cell_validity={nil} />
          </div>
      <% end %>
    </div>
    """
  end
end

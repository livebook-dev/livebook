defmodule LivebookWeb.Output.FrameComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, counter: 0)}
  end

  @impl true
  def update(assigns, socket) do
    {update_type, assigns} = Map.pop(assigns, :update_type, nil)
    {outputs, assigns} = Map.pop!(assigns, :outputs)

    socket = assign(socket, assigns)

    socket =
      case update_type do
        nil ->
          assign(socket, outputs: outputs)

        :replace ->
          socket |> assign(outputs: outputs) |> update(:counter, &(&1 + 1))

        :append ->
          update(socket, :outputs, &(&1 ++ outputs))
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={"frame-output-#{@id}"}>
      <%= if @outputs != [] do %>
        <LivebookWeb.Output.outputs
          id={"frame-output-#{@id}-outputs"}
          output_views={output_views(@outputs, @id, @counter)}
          socket={@socket}
          session_id={@session_id}
          input_values={@input_values}
          runtime={nil}
          cell_validity_status={nil} />
      <% else %>
        <div class="text-gray-300">
          Empty output frame
        </div>
      <% end %>
    </div>
    """
  end

  defp output_views(outputs, id, counter) do
    outputs
    |> Enum.with_index()
    |> Enum.map(fn {output, idx} ->
      %{id: "#{id}-#{counter}-#{idx}", output: output}
    end)
    |> Enum.reverse()
  end
end

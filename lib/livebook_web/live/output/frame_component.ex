defmodule LivebookWeb.Output.FrameComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, counter: 0, empty: true)}
  end

  @impl true
  def update(assigns, socket) do
    {update_type, assigns} = Map.pop(assigns, :update_type, nil)
    {outputs, assigns} = Map.pop!(assigns, :outputs)

    socket = assign(socket, assigns)

    socket =
      if socket.assigns.counter == 0 do
        assign(socket, empty: outputs == [], counter: 1)
      else
        socket
      end

    socket =
      case update_type do
        nil ->
          assign(socket, outputs: outputs)

        :replace ->
          socket
          |> assign(outputs: outputs, empty: outputs == [])
          |> update(:counter, &(&1 + 1))

        :append ->
          socket
          |> assign(empty: false)
          |> update(:outputs, &(outputs ++ &1))
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={"frame-output-#{@id}"}>
      <%= if @empty do %>
        <div class="text-gray-300 p-4 rounded-lg border border-gray-200">
          Empty output frame
        </div>
      <% else %>
        <div id={"frame-outputs-#{@id}-#{@counter}"} phx-update="append">
          <LivebookWeb.Output.outputs
            outputs={@outputs}
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

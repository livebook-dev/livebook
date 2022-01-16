defmodule LivebookWeb.Output.FrameDynamicLive do
  use LivebookWeb, :live_view

  @impl true
  def mount(
        _params,
        %{
          "pid" => pid,
          "id" => id,
          "session_id" => session_id,
          "input_values" => input_values,
          "cell_validity_status" => cell_validity_status
        },
        socket
      ) do
    if connected?(socket) do
      send(pid, {:connect, self()})
    end

    {:ok,
     assign(socket,
       id: id,
       output: nil,
       session_id: session_id,
       input_values: input_values,
       cell_validity_status: cell_validity_status
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <%= if @output do %>
        <LivebookWeb.Output.outputs
          outputs={[{"#{@id}-output", @output}]}
          socket={@socket}
          session_id={@session_id}
          runtime={nil}
          input_values={@input_values}
          cell_validity_status={@cell_validity_status} />
      <% else %>
        <div class="text-gray-300">
          Empty output frame
        </div>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_info({:connect_reply, %{output: output}}, socket) do
    {:noreply, assign(socket, output: output)}
  end

  def handle_info({:render, %{output: output}}, socket) do
    {:noreply, assign(socket, output: output)}
  end
end

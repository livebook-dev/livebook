defmodule LivebookWeb.Output.FrameDynamicLive do
  use LivebookWeb, :live_view

  @impl true
  def mount(_params, %{"pid" => pid, "id" => id}, socket) do
    send(pid, {:connect, self()})

    {:ok, assign(socket, id: id, output: nil)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <%= if @output do %>
        <%= LivebookWeb.Output.render_output(@output, %{
              id: "#{@id}-frame",
              socket: @socket,
              runtime: nil
            }) %>
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

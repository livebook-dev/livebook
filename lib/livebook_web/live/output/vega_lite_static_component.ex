defmodule LivebookWeb.Output.VegaLiteStaticComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, socket}
  end

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, id: assigns.id)
    {:ok, push_event(socket, "vega_lite:#{socket.assigns.id}:init", %{"spec" => assigns.spec})}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div id="vega-lite-<%= @id %>" phx-hook="VegaLite" phx-update="ignore" data-id="<%= @id %>">
    </div>
    """
  end
end

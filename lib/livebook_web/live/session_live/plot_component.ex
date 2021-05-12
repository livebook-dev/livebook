defmodule LivebookWeb.SessionLive.PlotComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, socket}
  end

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, id: assigns.id)
    {:ok, push_event(socket, "plot:#{socket.assigns.id}", %{"spec" => assigns.spec})}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div id="<%= @id %>" phx-hook="Plot" phx-update="ignore" data-id="<%= @id %>">
    </div>
    """
  end
end

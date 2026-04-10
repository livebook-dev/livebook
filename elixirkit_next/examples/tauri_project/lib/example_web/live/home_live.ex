defmodule ExampleWeb.HomeLive do
  use ExampleWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, count: 0)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex items-center justify-between m-4">
      <div class="flex items-center gap-2">
        <span>Count: <span class="font-mono">{@count}</span></span>
        <button phx-click="inc" class="btn btn-sm btn-outline">+</button>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("inc", _params, socket) do
    count = socket.assigns.count + 1
    ElixirKit.PubSub.broadcast("messages", "count:#{count}")
    {:noreply, assign(socket, count: count)}
  end
end

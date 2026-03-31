defmodule ExampleWeb.HomeLive do
  use ExampleWeb, :live_view

  def mount(_params, _session, socket) do
    {:ok, assign(socket, count: 0)}
  end

  def render(assigns) do
    ~H"""
    <div class="flex items-center justify-between m-4">
      <div class="flex items-center gap-2">
        <span>Count: <span class="font-mono">{@count}</span></span>
        <button phx-click="inc" class="btn btn-sm btn-outline">+</button>
      </div>
      <button
        :if={devtools_enabled?()}
        phx-click="toggle_devtools"
        class="btn btn-sm btn-outline rounded-full"
      >
        Dev Tools
      </button>
    </div>
    """
  end

  if Mix.env() == :prod do
    defp devtools_enabled?, do: false
  else
    defp devtools_enabled?, do: true
  end

  def handle_event("inc", _params, socket) do
    count = socket.assigns.count + 1
    ElixirKit.PubSub.broadcast("messages", "count:#{count}")
    {:noreply, assign(socket, count: count)}
  end

  def handle_event("toggle_devtools", _params, socket) do
    ElixirKit.PubSub.broadcast("messages", "toggle_devtools")
    {:noreply, socket}
  end
end

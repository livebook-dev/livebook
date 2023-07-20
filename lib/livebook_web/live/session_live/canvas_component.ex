defmodule LivebookWeb.SessionLive.CanvasComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  @impl true
  def mount(socket) do
    {:ok, socket}
  end

  @impl true
  def update(assigns, socket) do
    socket =
      socket
      |> assign(
        session: assigns.session,
        client_id: assigns.client_id,
        canvas_layout: assigns.canvas_layout
      )
      |> push_event("reload", %{payload: assigns.canvas_layout})

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id="gridstack-container" phx-update="ignore">
      <div
        id="canvas-grid"
        class="grid-stack"
        gs-column="12"
        phx-hook="Canvas"
        data-phx-target={@myself}
      />
    </div>
    """
  end

  @impl true
  def handle_event("items_changed", params, socket) do
    items = params |> Enum.map(fn {k, v} -> {k, convert(v)} end) |> Enum.into(%{})
    IO.inspect(items, label: "ITEMS CHANGED")
    Session.update_canvas(socket.assigns.session.pid, items)
    {:noreply, socket}
  end

  # TODO rename/optimize
  defp convert(value) when is_map(value) do
    Enum.into(value, %{}, fn {k, v} -> {String.to_existing_atom(k), v} end)
  end

  defp convert(value), do: value
end

defmodule LivebookWeb.Kino.VegaLiteLive do
  use LivebookWeb, :live_view

  @impl true
  def mount(_params, %{"pid" => pid, "id" => id}, socket) do
    send(pid, {:connect, self()})

    {:ok, assign(socket, id: id)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div id="vega-lite-<%= @id %>" phx-hook="VegaLite" phx-update="ignore" data-id="<%= @id %>">
    </div>
    """
  end

  @impl true
  def handle_info({:connect_reply, %{spec: spec}}, socket) do
    {:noreply, push_event(socket, "vega_lite:#{socket.assigns.id}:init", %{"spec" => spec})}
  end

  def handle_info({:push, %{data: data, dataset: dataset, window: window}}, socket) do
    {:noreply,
     push_event(socket, "vega_lite:#{socket.assigns.id}:push", %{
       "data" => data,
       "dataset" => dataset,
       "window" => window
     })}
  end
end

defmodule LivebookWeb.Output.JSDynamicLive do
  use LivebookWeb, :live_view

  @impl true
  def mount(
        _params,
        %{"pid" => pid, "id" => id, "info" => info, "session_id" => session_id},
        socket
      ) do
    if connected?(socket) do
      send(pid, {:connect, self(), %{origin: self()}})
    end

    assets_base_url = Routes.session_url(socket, :show_asset, session_id, info.assets.hash, [])

    {:ok,
     assign(socket,
       widget_pid: pid,
       id: id,
       assets_base_url: assets_base_url,
       js_path: info.assets.js_path
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={"js-output-#{@id}"}
      phx-hook="JSOutput"
      phx-update="ignore"
      data-id={@id}
      data-assets-base-url={@assets_base_url}
      data-js-path={@js_path}>
    </div>
    """
  end

  @impl true
  def handle_event("event", %{"event" => event, "payload" => payload}, socket) do
    send(socket.assigns.widget_pid, {:event, event, payload, %{origin: self()}})
    {:noreply, socket}
  end

  @impl true
  def handle_info({:connect_reply, data}, socket) do
    {:noreply, push_event(socket, "js_output:#{socket.assigns.id}:init", %{"data" => data})}
  end

  def handle_info({:event, event, payload}, socket) do
    {:noreply,
     push_event(socket, "js_output:#{socket.assigns.id}:event", %{
       "event" => event,
       "payload" => payload
     })}
  end
end

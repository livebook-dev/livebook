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
       error: nil,
       widget_pid: pid,
       id: id,
       assets_base_url: assets_base_url,
       js_path: info.assets.js_path
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <%= if @error do %>
        <div class="error-box">
          <%= @error %>
        </div>
      <% else %>
        <div id={"js-output-#{@id}"}
          phx-hook="JSOutput"
          phx-update="ignore"
          data-id={@id}
          data-assets-base-url={@assets_base_url}
          data-js-path={@js_path}>
        </div>
      <% end %>
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
    socket =
      case validate_json_serializability(data) do
        :ok ->
          push_event(socket, "js_output:#{socket.assigns.id}:init", %{"data" => data})

        {:error, error} ->
          assign(socket, error: "Failed to serialize initial widget data, " <> error)
      end

    {:noreply, socket}
  end

  def handle_info({:event, event, payload}, socket) do
    socket =
      case validate_json_serializability(payload) do
        :ok ->
          push_event(socket, "js_output:#{socket.assigns.id}:event", %{
            "event" => event,
            "payload" => payload
          })

        {:error, error} ->
          assign(socket, error: "Failed to serialize event payload, " <> error)
      end

    {:noreply, socket}
  end
end

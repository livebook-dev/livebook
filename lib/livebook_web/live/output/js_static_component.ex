defmodule LivebookWeb.Output.JSStaticComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, initialized: false, error: nil)}
  end

  @impl true
  def update(assigns, socket) do
    assets_base_url =
      Routes.session_url(socket, :show_asset, assigns.session_id, assigns.info.assets.hash, [])

    socket =
      assign(socket,
        id: assigns.id,
        assets_base_url: assets_base_url,
        js_path: assigns.info.assets.js_path
      )

    socket =
      if connected?(socket) and not socket.assigns.initialized do
        socket = assign(socket, initialized: true)

        case validate_json_serializability(assigns.data) do
          :ok ->
            push_event(socket, "js_output:#{socket.assigns.id}:init", %{"data" => assigns.data})

          {:error, error} ->
            assign(socket, error: "Failed to serialize widget data, " <> error)
        end
      else
        socket
      end

    {:ok, socket}
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
end

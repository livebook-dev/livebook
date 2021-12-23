defmodule LivebookWeb.Output.JSStaticComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, initialized: false)}
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
        socket
        |> assign(initialized: true)
        |> push_event("js_output:#{socket.assigns.id}:init", %{"data" => assigns.data})
      else
        socket
      end

    {:ok, socket}
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
end

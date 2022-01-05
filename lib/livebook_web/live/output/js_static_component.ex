defmodule LivebookWeb.Output.JSStaticComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, initialized: false)}
  end

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    socket =
      if connected?(socket) and not socket.assigns.initialized do
        socket
        |> assign(initialized: true)
        # TODO validate serialization
        |> push_event("js_output:#{socket.assigns.info.ref}:init", %{"data" => assigns.data})
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
      data-ref={@info.ref}
      data-assets-base-url={Routes.session_url(@socket, :show_asset, @session_id, @info.assets.hash, [])}
      data-js-path={@info.assets.js_path}>
    </div>
    """
  end
end

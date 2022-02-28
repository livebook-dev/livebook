defmodule LivebookWeb.JSViewComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div id={"js-output-#{@id}-#{@js_view.ref}"}
      phx-hook="JSView"
      phx-update="ignore"
      data-ref={@js_view.ref}
      data-assets-base-path={Routes.session_path(@socket, :show_asset, @session_id, @js_view.assets.hash, [])}
      data-js-path={@js_view.assets.js_path}
      data-session-token={session_token(@js_view.pid)}
      data-session-id={@session_id}
      data-iframe-local-port={LivebookWeb.IframeEndpoint.port()}>
    </div>
    """
  end

  defp session_token(pid) do
    Phoenix.Token.sign(LivebookWeb.Endpoint, "js view", %{pid: pid})
  end
end

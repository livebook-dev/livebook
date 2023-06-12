defmodule LivebookWeb.JSViewComponent do
  use LivebookWeb, :live_component

  @impl true
  def update(assigns, socket) do
    {:ok,
     socket
     |> assign(assigns)
     |> assign_new(:timeout_message, fn -> "Not available" end)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id={"js-output-#{@id}-#{@js_view.ref}"}
      phx-hook="JSView"
      phx-update="ignore"
      data-ref={@js_view.ref}
      data-assets-base-path={~p"/public/sessions/#{@session_id}/assets/#{@js_view.assets.hash}/"}
      data-assets-cdn-url={cdn_url(@js_view.assets[:cdn_url])}
      data-js-path={@js_view.assets.js_path}
      data-session-token={session_token(@session_id, @client_id)}
      data-connect-token={connect_token(@js_view.pid)}
      data-iframe-local-port={LivebookWeb.IframeEndpoint.port()}
      data-iframe-url={Livebook.Config.iframe_url()}
      data-timeout-message={@timeout_message}
    >
    </div>
    """
  end

  defp cdn_url(nil), do: nil
  defp cdn_url(url), do: url <> "/"

  defp session_token(session_id, client_id) do
    Phoenix.Token.sign(LivebookWeb.Endpoint, "session", %{
      session_id: session_id,
      client_id: client_id
    })
  end

  defp connect_token(pid) do
    Phoenix.Token.sign(LivebookWeb.Endpoint, "js-view-connect", %{pid: pid})
  end
end

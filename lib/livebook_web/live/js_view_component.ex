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
      data-p-ref={hook_prop(@js_view.ref)}
      data-p-assets-base-path={
        hook_prop(~p"/public/sessions/#{@session_id}/assets/#{@js_view.assets.hash}/")
      }
      data-p-assets-cdn-url={hook_prop(cdn_url(@js_view.assets[:cdn_url]))}
      data-p-js-path={hook_prop(@js_view.assets.js_path)}
      data-p-session-token={hook_prop(session_token(@session_id, @client_id))}
      data-p-connect-token={hook_prop(connect_token(@js_view.pid))}
      data-p-iframe-port={hook_prop(LivebookWeb.IframeEndpoint.port())}
      data-p-iframe-url={hook_prop(Livebook.Config.iframe_url())}
      data-p-timeout-message={hook_prop(@timeout_message)}
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

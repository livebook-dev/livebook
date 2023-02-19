defmodule LivebookWeb.Socket do
  use Phoenix.Socket

  channel "js_view", LivebookWeb.JSViewChannel

  @impl true
  def connect(_params, socket, info) do
    # The session is present only if the CSRF token is valid. We rely
    # on CSRF token, because we don't check connection origin as noted
    # in LivebookWeb.Endpoint.
    if info.session do
      {:ok, socket}
    else
      :error
    end
  end

  @impl true
  def id(_socket), do: nil
end

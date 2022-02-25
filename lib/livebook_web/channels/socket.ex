defmodule LivebookWeb.Socket do
  use Phoenix.Socket

  channel "js_view", LivebookWeb.JSViewChannel

  @impl true
  def connect(_params, socket, info) do
    auth_mode = Livebook.Config.auth_mode()

    if LivebookWeb.AuthPlug.authenticated?(info.session || %{}, info.uri.port, auth_mode) do
      {:ok, socket}
    else
      :error
    end
  end

  @impl true
  def id(_socket), do: nil
end

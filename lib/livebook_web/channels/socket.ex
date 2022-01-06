defmodule LivebookWeb.Socket do
  use Phoenix.Socket

  # App channels
  channel "js_output", LivebookWeb.JSOutputChannel

  # LiveView channels
  channel "lvu:*", Phoenix.LiveView.UploadChannel
  channel "lv:*", Phoenix.LiveView.Channel

  @impl true
  def connect(params, socket, info) do
    auth_mode = Livebook.Config.auth_mode()

    if LivebookWeb.AuthPlug.authenticated?(info.session || %{}, info.uri.port, auth_mode) do
      Phoenix.LiveView.Socket.connect(params, socket, info)
    else
      :error
    end
  end

  @impl true
  def id(socket) do
    Phoenix.LiveView.Socket.id(socket)
  end
end

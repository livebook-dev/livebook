defmodule LivebookWeb.SidebarHook do
  require Logger

  import Phoenix.Component
  import Phoenix.LiveView

  def on_mount(:default, _params, _session, socket) do
    if connected?(socket) do
      Livebook.Hubs.subscribe([:crud, :connection])
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sidebar")
    end

    socket =
      socket
      |> assign(saved_hubs: Livebook.Hubs.get_metadatas())
      |> attach_hook(:hubs, :handle_info, &handle_info/2)
      |> attach_hook(:shutdown, :handle_info, &handle_info/2)
      |> attach_hook(:shutdown, :handle_event, &handle_event/3)

    {:cont, socket}
  end

  defp handle_info(:shutdown, socket) do
    {:halt, put_flash(socket, :info, "Livebook is shutting down. You can close this page.")}
  end

  @connection_events ~w(hub_connected hub_disconnected hubs_metadata_changed)a

  defp handle_info(event, socket) when event in @connection_events do
    {:halt, assign(socket, saved_hubs: Livebook.Hubs.get_metadatas())}
  end

  @error_events ~w(hub_connection_failed hub_disconnection_failed)a

  defp handle_info({event, _reason}, socket) when event in @error_events do
    {:halt, assign(socket, saved_hubs: Livebook.Hubs.get_metadatas())}
  end

  defp handle_info(_event, socket), do: {:cont, socket}

  defp handle_event("shutdown", _params, socket) do
    Livebook.Config.shutdown()
    {:halt, socket}
  end

  defp handle_event(_event, _params, socket), do: {:cont, socket}
end

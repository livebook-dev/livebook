defmodule LivebookWeb.SidebarHook do
  import Phoenix.LiveView

  def on_mount(:default, _params, _session, socket) do
    if connected?(socket) do
      Livebook.Hubs.subscribe()
    end

    socket =
      socket
      |> assign(saved_hubs: Livebook.Hubs.fetch_metadatas())
      |> attach_hook(:hubs, :handle_info, &handle_info/2)
      |> attach_hook(:shutdown, :handle_event, &handle_event/3)

    {:cont, socket}
  end

  defp handle_info({:hubs_metadata_changed, hubs}, socket) do
    {:halt, assign(socket, :saved_hubs, hubs)}
  end

  defp handle_info(_event, socket), do: {:cont, socket}

  defp handle_event("shutdown", _params, socket) do
    if Livebook.Config.shutdown_enabled?() do
      System.stop()
      {:halt, put_flash(socket, :info, "Livebook is shutting down. You can close this page.")}
    else
      socket
    end
  end

  defp handle_event(_event, _params, socket), do: {:cont, socket}
end

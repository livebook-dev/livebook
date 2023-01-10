defmodule LivebookWeb.SidebarHook do
  require Logger

  import Phoenix.Component
  import Phoenix.LiveView

  def on_mount(:default, _params, _session, socket) do
    if connected?(socket) do
      Livebook.Hubs.subscribe([:crud, :connection])
    end

    socket =
      socket
      |> assign(saved_hubs: Livebook.Hubs.get_metadatas())
      |> attach_hook(:hubs, :handle_info, &handle_info/2)
      |> attach_hook(:shutdown, :handle_event, &handle_event/3)

    {:cont, socket}
  end

  defp handle_info({:hubs_metadata_changed, hubs}, socket) do
    {:cont, assign(socket, saved_hubs: hubs)}
  end

  defp handle_info({:connect, _, _}, socket) do
    {:cont, assign(socket, saved_hubs: Livebook.Hubs.get_metadatas())}
  end

  defp handle_info({:disconnect, _, _}, socket) do
    {:cont, assign(socket, saved_hubs: Livebook.Hubs.get_metadatas())}
  end

  defp handle_info(_event, socket), do: {:cont, socket}

  defp handle_event("shutdown", _params, socket) do
    case Livebook.Config.shutdown_callback() do
      {m, f, a} ->
        apply(m, f, a)

        {:halt, put_flash(socket, :info, "Livebook is shutting down. You can close this page.")}

      _ ->
        socket
    end
  end

  defp handle_event(_event, _params, socket), do: {:cont, socket}
end

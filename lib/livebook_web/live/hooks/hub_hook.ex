defmodule LivebookWeb.HubHook do
  import Phoenix.LiveView

  def on_mount(:default, _params, _session, socket) do
    if connected?(socket) do
      Livebook.HubProvider.Settings.subscribe()
    end

    socket =
      socket
      |> assign(saved_hubs: Livebook.HubProvider.Settings.fetch_hubs())
      |> attach_hook(:hubs_subscription, :handle_info, &handle_info/2)

    {:cont, socket}
  end

  defp handle_info({:hubs_changed, hubs}, socket) do
    {:cont, assign(socket, :saved_hubs, hubs)}
  end

  defp handle_info(_message, socket), do: {:cont, socket}
end

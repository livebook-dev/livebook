defmodule LivebookWeb.SidebarHook do
  require Logger

  import Phoenix.Component
  import Phoenix.LiveView

  alias Livebook.Hubs.Enterprise
  alias Livebook.Hubs.EnterpriseClient

  def on_mount(:default, _params, _session, socket) do
    if connected?(socket) do
      Livebook.Hubs.subscribe()
    end

    socket =
      socket
      |> assign(saved_hubs: Livebook.Hubs.fetch_metadatas())
      |> connect_enterprise_hubs()
      |> attach_hook(:hubs, :handle_info, &handle_info/2)
      |> attach_hook(:shutdown, :handle_event, &handle_event/3)
      |> assign_new(:connected_hubs, fn _ -> [] end)

    {:cont, socket}
  end

  defp handle_info({:hubs_metadata_changed, hubs}, socket) do
    {:halt,
     socket
     |> assign(saved_hubs: hubs)
     |> connect_enterprise_hubs()}
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

  @supervisor Livebook.HubsSupervisor
  @registry Livebook.HubsRegistry

  defp connect_enterprise_hubs(socket) do
    for %{provider: %Enterprise{} = enterprise} <- socket.assigns.saved_hubs, reduce: socket do
      acc ->
        pid =
          case Registry.lookup(@registry, enterprise.url) do
            [{pid, _}] ->
              pid

            [] ->
              case DynamicSupervisor.start_child(@supervisor, {EnterpriseClient, enterprise}) do
                {:ok, pid} -> pid
                {:error, {:already_started, pid}} -> pid
              end
          end

        add_connected_hub(acc, %{hub: enterprise, pid: pid})
    end
  end

  defp add_connected_hub(socket, connected_hub) when length(socket.assigns.connected_hubs) >= 1 do
    if Enum.find(socket.assigns.connected_hubs, &(&1.hub == connected_hub.hub)) do
      socket
    else
      assign(socket, :connected_hubs, [connected_hub | socket.assigns.connected_hubs])
    end
  end

  defp add_connected_hub(socket, connected_hub) do
    assign(socket, :connected_hubs, [connected_hub])
  end
end

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

    hubs = Livebook.Hubs.fetch_metadatas()

    socket =
      socket
      |> assign(saved_hubs: hubs)
      |> attach_hook(:hubs, :handle_info, &handle_info/2)
      |> attach_hook(:shutdown, :handle_event, &handle_event/3)

    {:cont, assign(socket, connected_hubs: connect_enterprise_hubs(hubs))}
  end

  defp handle_info({:hubs_metadata_changed, hubs}, socket) do
    {:halt, assign(socket, saved_hubs: hubs, connected_hubs: connect_enterprise_hubs(hubs))}
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

  # TODO: Move Hub connection life-cycle elsewhere
  @supervisor Livebook.HubsSupervisor
  @registry Livebook.HubsRegistry

  defp connect_enterprise_hubs(hubs) do
    for %{provider: %Enterprise{} = enterprise} <- hubs do
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

      %{hub: enterprise, pid: pid}
    end
  end
end

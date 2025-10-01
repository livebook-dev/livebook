defmodule Livebook.FileSystem.Mounter do
  # This server is responsible to handle file systems that are mountable
  use GenServer
  require Logger

  alias Livebook.{FileSystem, Hubs}

  @name __MODULE__
  @loop_delay to_timeout(hour: 1)

  def start_link(opts \\ []) do
    {name, opts} = Keyword.pop(opts, :name, @name)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  if Mix.env() == :test do
    def subscribe(hub_id) do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "file_systems:#{hub_id}")
    end
  end

  @impl GenServer
  def init(opts) do
    loop_delay = Keyword.get(opts, :loop_delay, @loop_delay)
    {:ok, %{hubs: %{}, loop_delay: loop_delay}, {:continue, :boot}}
  end

  @impl GenServer
  def handle_continue(:boot, state) do
    Hubs.Broadcasts.subscribe([:crud, :file_systems, :connection])
    Process.send_after(self(), :remount, state.loop_delay)

    {:noreply, mount_file_systems(state, Hubs.Personal.id())}
  end

  @impl GenServer
  def handle_info({:hub_connected, hub_id}, state) do
    {:noreply, mount_file_systems(state, hub_id)}
  end

  def handle_info({:file_system_created, file_system}, state) do
    {:noreply, mount_file_system(state, file_system)}
  end

  def handle_info({:file_system_updated, file_system}, state) do
    {:noreply, mount_file_system(state, file_system)}
  end

  def handle_info({:file_system_deleted, file_system}, state) do
    {:noreply, unmount_file_system(state, file_system)}
  end

  def handle_info({:hub_deleted, hub_id}, state) do
    {:noreply, unmount_file_systems(state, hub_id)}
  end

  def handle_info(:remount, state) do
    Process.send_after(self(), :remount, state.loop_delay)
    {:noreply, remount_file_systems(state)}
  end

  def handle_info(_message, state), do: {:noreply, state}

  defp mount_file_systems(state, hub_id) do
    case Hubs.fetch_hub(hub_id) do
      {:ok, hub} ->
        hub
        |> Hubs.get_file_systems(hub_only: true)
        |> Enum.reduce(put_hub(state, hub.id), &mount_file_system(&2, &1))

      :error ->
        unmount_file_systems(state, hub_id)
    end
  end

  defp remount_file_systems(state) do
    state.hubs
    |> Map.keys()
    |> Enum.reduce(state, &mount_file_systems(&2, &1))
  end

  defp unmount_file_systems(state, hub_id) do
    if metadata = state.hubs[hub_id] do
      metadata.file_systems
      |> Enum.reduce(state, &unmount_file_system(&2, &1))
      |> remove_hub(hub_id)
    else
      state
    end
  end

  defp mount_file_system(state, file_system) do
    case FileSystem.mount(file_system) do
      :ok ->
        broadcast({:file_system_mounted, file_system})
        put_hub_file_system(state, file_system)

      {:error, reason} ->
        Logger.error("[file_system=#{name(file_system)}] failed to mount: #{reason}")
        Process.send_after(self(), {:file_system_created, file_system}, to_timeout(second: 10))

        state
    end
  end

  defp unmount_file_system(state, file_system) do
    case FileSystem.unmount(file_system) do
      :ok ->
        broadcast({:file_system_unmounted, file_system})
        remove_hub_file_system(state, file_system)

      {:error, reason} ->
        Logger.error("[file_system=#{name(file_system)}] failed to unmount: #{reason}")
        Process.send_after(self(), {:file_system_deleted, file_system}, to_timeout(second: 10))

        state
    end
  end

  defp put_hub_file_system(state, file_system) do
    if state.hubs[file_system.hub_id] do
      update_in(state.hubs[file_system.hub_id], &put_file_system(&1, file_system))
    else
      state
    end
  end

  defp remove_hub_file_system(state, file_system) do
    if state.hubs[file_system.hub_id] do
      update_in(state.hubs[file_system.hub_id], &remove_file_system(&1, file_system))
    else
      state
    end
  end

  defp put_hub(state, hub_id) do
    update_in(state.hubs, &Map.put_new(&1, hub_id, %{file_systems: []}))
  end

  defp put_file_system(hub_data, file_system) do
    hub_data = remove_file_system(hub_data, file_system)
    put_in(hub_data.file_systems, [file_system | hub_data.file_systems])
  end

  defp remove_hub(state, hub_id) do
    update_in(state.hubs, &Map.delete(&1, hub_id))
  end

  defp remove_file_system(hub_data, file_system) do
    file_systems = Enum.reject(hub_data.file_systems, &(&1.id == file_system.id))
    put_in(hub_data.file_systems, file_systems)
  end

  defp name(file_system) do
    FileSystem.external_metadata(file_system).name
  end

  if Mix.env() == :test do
    defp broadcast({_, %{external_id: _, hub_id: id}} = message) do
      Phoenix.PubSub.broadcast(Livebook.PubSub, "file_systems:#{id}", message)
    end
  else
    defp broadcast(_), do: :ok
  end
end

defmodule Livebook.FileSystem.Mounter do
  # This server is responsible to handle file systems that are mountable
  use GenServer

  alias Livebook.{FileSystem, Hubs}

  @name __MODULE__
  @loop_delay 60 * 60

  def start_link(opts \\ []) do
    {name, opts} = Keyword.pop(opts, :name, @name)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @impl GenServer
  def init(opts) do
    loop_delay = Keyword.get(opts, :loop_delay, @loop_delay)
    {:ok, %{hubs: %{}, loop_delay: loop_delay}, {:continue, :boot}}
  end

  @impl GenServer
  def handle_continue(:boot, state) do
    Hubs.Broadcasts.subscribe([:connection, :crud, :file_systems])
    {:noreply, init_hub_file_systems(state, Hubs.Personal.id())}
  end

  @impl GenServer
  def handle_info({:hub_connected, hub_id}, state) do
    {:noreply, init_hub_file_systems(state, hub_id)}
  end

  def handle_info({:file_system_created, file_system}, state) do
    {:noreply, mount_file_system(state, file_system)}
  end

  def handle_info({:file_system_updated, file_system}, state) do
    {:noreply, update_file_system(state, file_system)}
  end

  def handle_info({:file_system_deleted, file_system}, state) do
    {:noreply, umount_file_system(state, file_system)}
  end

  def handle_info({:hub_changed, hub_id}, state) do
    if not Map.has_key?(state.hubs, hub_id) do
      {:noreply, init_hub_file_systems(state, hub_id)}
    else
      {:noreply, state}
    end
  end

  def handle_info({:hub_deleted, hub_id}, state) do
    {:noreply, umount_hub_file_systems(state, hub_id)}
  end

  def handle_info({:update_file_system, file_system}, state) do
    {:noreply, update_file_system(state, file_system)}
  end

  def handle_info(_message, state), do: {:noreply, state}

  defp init_hub_file_systems(state, hub_id) do
    case Hubs.fetch_hub(hub_id) do
      {:ok, hub} ->
        file_systems = get_mountable_file_systems(hub)
        dispatch_file_systems(:file_system_created, file_systems)
        %{state | hubs: Map.put(state.hubs, hub_id, %{file_systems: [], versions: %{}})}

      :error ->
        send(self(), {:hub_deleted, hub_id})
        state
    end
  end

  defp umount_hub_file_systems(state, hub_id) do
    if metadata = state.hubs[hub_id] do
      state = Enum.reduce(metadata.file_systems, state, &umount_file_system(&2, &1))
      %{state | hubs: Map.delete(state.hubs, hub_id)}
    else
      state
    end
  end

  def get_mountable_file_systems(hub) do
    hub
    |> Hubs.get_file_systems(hub_only: true)
    |> Enum.filter(&FileSystem.mountable?/1)
  end

  defp dispatch_file_systems(event, file_systems) do
    for file_system <- file_systems do
      send(self(), {event, file_system})
    end
  end

  defp updatable?(state, file_system) do
    case Map.get(state.hubs, file_system.hub_id) do
      %{versions: versions} ->
        if version = versions[file_system.id] do
          :erlang.phash2(file_system) != version
        else
          false
        end

      _otherwise ->
        false
    end
  end

  defp mount_file_system(state, file_system) do
    cond do
      not Map.has_key?(state.hubs, file_system.hub_id) ->
        init_hub_file_systems(state, file_system.hub_id)

      FileSystem.mountable?(file_system) and not FileSystem.mounted?(file_system) ->
        do_mount_file_system(state, file_system)

      true ->
        state
    end
  end

  defp do_mount_file_system(state, file_system) do
    case FileSystem.mount(file_system) do
      :ok ->
        Hubs.Broadcasts.file_system_mounted(file_system)
        Process.send_after(self(), {:update_file_system, file_system}, state.loop_delay)
        metadata = Map.fetch!(state.hubs, file_system.hub_id)

        metadata = %{
          metadata
          | file_systems: [file_system | metadata.file_systems],
            versions: Map.put_new(metadata.versions, file_system.id, :erlang.phash2(file_system))
        }

        %{state | hubs: Map.replace!(state.hubs, file_system.hub_id, metadata)}

      {:error, _reason} ->
        state
    end
  end

  defp update_file_system(state, file_system) do
    cond do
      not Map.has_key?(state.hubs, file_system.hub_id) ->
        init_hub_file_systems(state, file_system.hub_id)

      FileSystem.mountable?(file_system) and FileSystem.mounted?(file_system) and
          updatable?(state, file_system) ->
        do_update_file_system(state, file_system)

      true ->
        state
    end
  end

  defp do_update_file_system(state, file_system) do
    case FileSystem.remount(file_system) do
      :ok ->
        Hubs.Broadcasts.file_system_mounted(file_system)
        Process.send_after(self(), {:update_file_system, file_system}, state.loop_delay)
        metadata = Map.fetch!(state.hubs, file_system.hub_id)

        metadata = %{
          metadata
          | file_systems: [
              file_system | Enum.reject(metadata.file_systems, &(&1.id == file_system.id))
            ],
            versions: Map.replace!(metadata.versions, file_system.id, :erlang.phash2(file_system))
        }

        %{state | hubs: Map.replace!(state.hubs, file_system.hub_id, metadata)}

      {:error, _reason} ->
        state
    end
  end

  defp umount_file_system(state, file_system) do
    if Map.has_key?(state.hubs, file_system.hub_id) and FileSystem.mountable?(file_system) and
         FileSystem.mounted?(file_system) do
      do_umount_file_system(state, file_system)
    else
      state
    end
  end

  defp do_umount_file_system(state, file_system) do
    case FileSystem.umount(file_system) do
      :ok ->
        Hubs.Broadcasts.file_system_umounted(file_system)
        metadata = Map.fetch!(state.hubs, file_system.hub_id)

        metadata = %{
          metadata
          | file_systems: Enum.reject(metadata.file_systems, &(&1.id == file_system.id)),
            versions: Map.delete(metadata.versions, file_system.id)
        }

        %{state | hubs: Map.replace!(state.hubs, file_system.hub_id, metadata)}

      {:error, _reason} ->
        state
    end
  end
end

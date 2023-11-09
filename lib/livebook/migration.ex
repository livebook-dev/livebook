defmodule Livebook.Migration do
  use GenServer, restart: :temporary

  # We version our storage so we can remove migrations in the future
  # by deleting the whole storage when finding too old versions.
  #
  # We also document tables and IDs used in previous versions,
  # as those must be avoided in the future.
  #
  # ## v1 (Livebook v0.11, Oct 2023)
  #
  # * Deleted hubs.local-host
  # * Migrated secrets to hub_secrets
  # * Migrated filesystems to file_systems
  # * Migrated settings.global.default_file_system_id to settings.global.default_dir
  # * Added :file_system_type to starred/recent files
  #
  @migration_version 1

  alias Livebook.Storage

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    insert_personal_hub()
    remove_offline_hub()

    # v1
    add_personal_hub_secret_key()
    delete_local_host_hub()
    move_app_secrets_to_personal_hub()
    add_file_system_type_to_notebook_manager_files()
    add_team_hub_offline()

    # TODO: remove on Livebook v0.12
    update_file_systems_to_deterministic_ids()
    ensure_new_file_system_attributes()
    move_default_file_system_id_to_default_dir()

    Storage.insert(:system, "global", migration_version: @migration_version)
    :ignore
  end

  defp delete_local_host_hub() do
    Storage.delete(:hubs, "local-host")
  end

  defp insert_personal_hub() do
    unless Livebook.Hubs.hub_exists?(Livebook.Hubs.Personal.id()) do
      Livebook.Hubs.save_hub(%Livebook.Hubs.Personal{
        id: Livebook.Hubs.Personal.id(),
        hub_name: "My Hub",
        hub_emoji: "🏠",
        secret_key: Livebook.Hubs.Personal.generate_secret_key()
      })
    end
  end

  defp remove_offline_hub() do
    # We put offline hub in the storage for consistency, but it should
    # be present if the environment variables are set. Consequently, we
    # always remove it and insert on startup if applicable.

    for %{id: "team-" <> _ = id} = attrs <- Storage.all(:hubs), offline_hub?(attrs) do
      :ok = Storage.delete(:hubs, id)
    end
  end

  # TODO: use just :offline on Livebook v0.12
  defp offline_hub?(%{offline: true}), do: true
  defp offline_hub?(%{user_id: 0, org_id: 0, org_key_id: 0}), do: true
  defp offline_hub?(_attrs), do: false

  defp move_app_secrets_to_personal_hub() do
    for %{name: name, value: value} <- Storage.all(:secrets) do
      secret = %Livebook.Secrets.Secret{
        name: name,
        value: value,
        hub_id: Livebook.Hubs.Personal.id()
      }

      Livebook.Hubs.Personal.set_secret(secret)
      Storage.delete(:secrets, name)
    end
  end

  defp add_personal_hub_secret_key() do
    with :error <- Storage.fetch_key(:hubs, Livebook.Hubs.Personal.id(), :secret_key) do
      secret_key = Livebook.Hubs.Personal.generate_secret_key()
      Storage.insert(:hubs, Livebook.Hubs.Personal.id(), secret_key: secret_key)
    end
  end

  # We changed S3 file system ids, such that they are deterministic
  # for the same bucket, rather than random. We take this opportunity
  # to rename the scope from :filesystem to :file_systems, which
  # conveniently allows for easy check if there's anything to migrate.
  # This migration can be removed in the future (at the cost of discarding
  # very old file systems (which can be re-added).
  # TODO: remove on Livebook v0.12
  defp update_file_systems_to_deterministic_ids() do
    case Storage.all(:filesystem) do
      [] ->
        :ok

      configs ->
        id_mapping =
          for config <- configs, into: %{} do
            old_id = config.id

            # Ensure new file system fields
            new_fields = %{
              hub_id: Livebook.Hubs.Personal.id(),
              external_id: nil,
              region: Livebook.FileSystem.S3.region_from_uri(config.bucket_url)
            }

            config = Map.merge(new_fields, config)

            # At this point S3 is the only file system we store
            file_system = Livebook.FileSystems.load("s3", config)
            Livebook.Hubs.Personal.save_file_system(file_system)
            Storage.delete(:filesystem, old_id)
            {old_id, file_system.id}
          end

        # Remap default file system id
        with {:ok, default_file_system_id} <-
               Storage.fetch_key(:settings, "global", :default_file_system_id),
             {:ok, new_id} <- Map.fetch(id_mapping, default_file_system_id) do
          Storage.insert(:settings, "global", default_file_system_id: new_id)
        end
    end
  end

  # Note that this is already handled by update_file_systems_to_deterministic_ids/0,
  # we add this migration so it also applies to people using Livebook main.
  # TODO: remove on Livebook v0.12
  defp ensure_new_file_system_attributes() do
    for attrs <- Storage.all(:file_systems) do
      new_attrs = %{
        hub_id: Livebook.Hubs.Personal.id(),
        external_id: nil,
        region: Livebook.FileSystem.S3.region_from_uri(attrs.bucket_url)
      }

      attrs = Map.merge(new_attrs, attrs)

      Storage.insert(:file_systems, attrs.id, Map.to_list(attrs))
    end
  end

  # TODO: remove on Livebook v0.12
  defp move_default_file_system_id_to_default_dir() do
    with {:ok, default_file_system_id} <-
           Storage.fetch_key(:settings, "global", :default_file_system_id) do
      Livebook.Hubs.get_file_systems()
      |> Enum.find(&(&1.id == default_file_system_id))
      |> Livebook.FileSystem.File.new()
      |> Livebook.Settings.set_default_dir()

      Storage.delete_key(:settings, "global", :default_file_system_id)
    end
  end

  # In the past, not all files had a file_system_type, so we need to detect one from the id.
  defp add_file_system_type_to_notebook_manager_files() do
    with {:ok, %{recent_notebooks: _, starred_notebooks: _} = attrs} <-
           Storage.fetch(:notebook_manager, "global") do
      attrs =
        attrs
        |> update_in([:recent_notebooks, Access.all(), :file], &add_file_system_type/1)
        |> update_in([:starred_notebooks, Access.all(), :file], &add_file_system_type/1)

      Storage.insert(:notebook_manager, "global", attrs)
    end
  end

  defp add_file_system_type(file) do
    file_system_type =
      Map.get_lazy(file, :file_system_type, fn ->
        case file.file_system_id do
          "local" -> "local"
          _ -> "s3"
        end
      end)

    Map.put(file, :file_system_type, file_system_type)
  end

  defp add_team_hub_offline() do
    for %{id: "team-" <> _ = id} = attrs <- Storage.all(:hubs),
        not Map.has_key?(attrs, :offline) do
      Storage.insert(:hubs, id, offline: false)
    end
  end
end

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
  #
  @migration_version 1

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    insert_personal_hub()

    # v1
    add_personal_hub_secret_key()
    delete_local_host_hub()
    move_app_secrets_to_personal_hub()

    # TODO: remove on Livebook v0.12
    update_file_systems_to_deterministic_ids()
    ensure_new_file_system_attributes()
    move_default_file_system_id_to_default_dir()

    Livebook.Storage.insert(:system, "global", migration_version: @migration_version)
    :ignore
  end

  defp delete_local_host_hub() do
    Livebook.Storage.delete(:hubs, "local-host")
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

  defp move_app_secrets_to_personal_hub() do
    for %{name: name, value: value} <- Livebook.Storage.all(:secrets) do
      secret = %Livebook.Secrets.Secret{
        name: name,
        value: value,
        hub_id: Livebook.Hubs.Personal.id()
      }

      Livebook.Hubs.Personal.set_secret(secret)
      Livebook.Storage.delete(:secrets, name)
    end
  end

  defp add_personal_hub_secret_key() do
    with :error <- Livebook.Storage.fetch_key(:hubs, Livebook.Hubs.Personal.id(), :secret_key) do
      secret_key = Livebook.Hubs.Personal.generate_secret_key()
      Livebook.Storage.insert(:hubs, Livebook.Hubs.Personal.id(), secret_key: secret_key)
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
    case Livebook.Storage.all(:filesystem) do
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
            Livebook.Storage.delete(:filesystem, old_id)
            {old_id, file_system.id}
          end

        # Remap default file system id
        with {:ok, default_file_system_id} <-
               Livebook.Storage.fetch_key(:settings, "global", :default_file_system_id),
             {:ok, new_id} <- Map.fetch(id_mapping, default_file_system_id) do
          Livebook.Storage.insert(:settings, "global", default_file_system_id: new_id)
        end
    end
  end

  # Note that this is already handled by update_file_systems_to_deterministic_ids/0,
  # we add this migration so it also applies to people using Livebook main.
  # TODO: remove on Livebook v0.12
  defp ensure_new_file_system_attributes() do
    for attrs <- Livebook.Storage.all(:file_systems) do
      new_attrs = %{
        hub_id: Livebook.Hubs.Personal.id(),
        external_id: nil,
        region: Livebook.FileSystem.S3.region_from_uri(attrs.bucket_url)
      }

      attrs = Map.merge(new_attrs, attrs)

      Livebook.Storage.insert(:file_systems, attrs.id, Map.to_list(attrs))
    end
  end

  # TODO: remove on Livebook v0.12
  defp move_default_file_system_id_to_default_dir() do
    with {:ok, default_file_system_id} <-
           Livebook.Storage.fetch_key(:settings, "global", :default_file_system_id) do
      Livebook.Hubs.get_file_systems()
      |> Enum.find(&(&1.id == default_file_system_id))
      |> Livebook.FileSystem.File.new()
      |> Livebook.Settings.set_default_dir()

      Livebook.Storage.delete_key(:settings, "global", :default_file_system_id)
    end
  end
end

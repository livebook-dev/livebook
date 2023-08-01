defmodule Livebook.Migration do
  @moduledoc false

  @doc """
  Runs all migrations.
  """
  @spec migrate() :: :ok
  def migrate() do
    delete_local_host_hub()
    insert_personal_hub()
    move_app_secrets_to_personal_hub()
    add_personal_hub_secret_key()
    update_file_systems_to_deterministic_ids()
    move_default_file_system_id_to_default_dir()
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

  defp update_file_systems_to_deterministic_ids() do
    # We changed S3 file system ids, such that they are deterministic
    # for the same bucket, rather than random. We take this opportunity
    # to rename the scope from :filesystem to :file_systems, which
    # conveniently allows for easy check if there's anything to migrate.
    # This migration can be removed in the future (at the cost of discarding
    # very old file systems (which can be re-added).
    # TODO: remove on Livebook v0.12

    case Livebook.Storage.all(:filesystem) do
      [] ->
        :ok

      configs ->
        id_mapping =
          for config <- configs, into: %{} do
            old_id = config.id
            # At this point S3 is the only file system we store
            {:ok, file_system} = Livebook.FileSystem.S3.from_config(config)
            Livebook.Settings.save_file_system(file_system)
            Livebook.Storage.delete(:filesystem, old_id)
            {old_id, file_system.id}
          end

        # Remap default file system id
        with {:ok, default_file_system_id} <-
               Livebook.Storage.fetch_key(:settings, "global", :default_file_system_id),
             {:ok, new_id} <- Map.fetch(id_mapping, default_file_system_id) do
          Livebook.Storage.insert(:settings, "global", default_file_system_id: new_id)
        end

        # Livebook.NotebookManager is started before the migration runs,
        # and it discards S3 files, since it can't find the file system.
        # However, in this case it's fine; for recent notebooks it does
        # not matter really and there are likely not many starred S3
        # notebooks at this point (and the user can easily star again)
    end
  end

  defp move_default_file_system_id_to_default_dir() do
    # Convert default_file_system_id to default_dir
    # TODO: remove on Livebook v0.12

    with {:ok, default_file_system_id} <-
           Livebook.Storage.fetch_key(:settings, "global", :default_file_system_id) do
      with {:ok, default_file_system} <-
             Livebook.Settings.fetch_file_system(default_file_system_id) do
        default_dir = Livebook.FileSystem.File.new(default_file_system)
        Livebook.Settings.set_default_dir(default_dir)
      end

      Livebook.Storage.delete_key(:settings, "global", :default_file_system_id)
    end
  end
end

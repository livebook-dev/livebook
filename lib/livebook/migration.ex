defmodule Livebook.Migration do
  alias Livebook.Storage

  # We version out storage, so that we know which migrations to run
  # when someone upgrades. In the future we can also remove migrations
  # by deleting the whole storage when the storage version is too old.
  @migration_version 1

  def migration_version(), do: @migration_version

  def run() do
    insert_personal_hub()
    remove_offline_hub()

    storage_version =
      case Storage.fetch_key(:system, "global", :migration_version) do
        {:ok, version} -> version
        :error -> 0
      end

    for version <- (storage_version + 1)..@migration_version//1 do
      migration(version)
    end

    Storage.insert(:system, "global", migration_version: @migration_version)
  end

  defp insert_personal_hub() do
    unless Livebook.Hubs.hub_exists?(Livebook.Hubs.Personal.id()) do
      Livebook.Hubs.save_hub(%Livebook.Hubs.Personal{
        id: Livebook.Hubs.Personal.id(),
        hub_name: "Personal",
        hub_emoji: "🏠",
        secret_key: Livebook.Hubs.Personal.generate_secret_key()
      })
    end
  end

  defp remove_offline_hub() do
    # We put offline hub in the storage for consistency, but it should
    # be present if the environment variables are set. Consequently, we
    # always remove it and insert on startup if applicable.

    for %{id: "team-" <> _ = id, offline: true} <- Storage.all(:hubs) do
      :ok = Storage.delete(:hubs, id)
    end
  end

  defp migration(1) do
    v1_add_personal_hub_secret_key()
    v1_delete_local_host_hub()
    v1_move_app_secrets_to_personal_hub()
    v1_add_file_system_type_to_notebook_manager_files()
    v1_remove_old_filesystems()
    v1_remove_default_file_system_id()
  end

  defp v1_delete_local_host_hub() do
    Storage.delete(:hubs, "local-host")
  end

  defp v1_move_app_secrets_to_personal_hub() do
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

  defp v1_add_personal_hub_secret_key() do
    with :error <- Storage.fetch_key(:hubs, Livebook.Hubs.Personal.id(), :secret_key) do
      secret_key = Livebook.Hubs.Personal.generate_secret_key()
      Storage.insert(:hubs, Livebook.Hubs.Personal.id(), secret_key: secret_key)
    end
  end

  # In the past, not all files had a file_system_type, so we need to detect one from the id.
  defp v1_add_file_system_type_to_notebook_manager_files() do
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

  defp v1_remove_old_filesystems() do
    # For a while we had a migration converting this to the new table.
    # Now we just delete the old entries for simplicity. If someone
    # upgrateds from an old version, they just need to re-add their
    # file systems. This has negligible impact.
    for id <- Storage.all(:filesystem), do: Storage.delete(:filesystem, id)
  end

  defp v1_remove_default_file_system_id() do
    # For a while we had a migration converting this to default dir.
    # Now we just delete the old setting for simplicity. If someone
    # upgrateds from an old version, they just need to set the default
    # fiel system again. This has negligible impact.
    Storage.delete_key(:settings, "global", :default_file_system_id)
  end
end

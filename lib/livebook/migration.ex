defmodule Livebook.Migration do
  @moduledoc false

  @doc """
  Runs all migrations.
  """
  @spec migrate() :: :ok
  def migrate() do
    insert_personal_hub()
    move_app_secrets_to_personal_hub()
    add_personal_hub_secret_key()
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
        hub_id: Livebook.Hubs.Personal.id(),
        readonly: false
      }

      Livebook.Secrets.set_secret(secret)
      Livebook.Storage.delete(:secrets, name)
    end
  end

  defp add_personal_hub_secret_key() do
    with :error <- Livebook.Storage.fetch_key(:hubs, Livebook.Hubs.Personal.id(), :secret_key) do
      secret_key = Livebook.Hubs.Personal.generate_secret_key()
      Livebook.Storage.insert(:hubs, Livebook.Hubs.Personal.id(), secret_key: secret_key)
    end
  end
end

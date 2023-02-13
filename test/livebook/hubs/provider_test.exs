defmodule Livebook.Hubs.ProviderTest do
  use Livebook.DataCase

  alias Livebook.Hubs.Provider
  alias Livebook.Secrets

  describe "personal" do
    setup do
      {:ok, hub: build(:personal)}
    end

    test "load/2", %{hub: hub} do
      assert Provider.load(hub, Map.from_struct(hub)) == hub
    end

    test "type/1", %{hub: hub} do
      assert Provider.type(hub) == "personal"
    end

    test "connection_spec/1", %{hub: hub} do
      refute Provider.connection_spec(hub)
    end

    test "disconnect/1", %{hub: hub} do
      assert Provider.disconnect(hub) == :ok
    end

    test "capabilities/1", %{hub: hub} do
      assert Provider.capabilities(hub) == [
               :connect,
               :list_secret,
               :create_secret,
               :delete_secret
             ]
    end

    test "get_secrets/1 without startup secrets", %{hub: hub} do
      secret = insert_secret(name: "GET_PERSONAL_SECRET", origin: {:hub, "personal-hub"})
      assert secret in Provider.get_secrets(hub)
    end

    test "get_secrets/1 with startup secrets", %{hub: hub} do
      secret = build(:secret, name: "GET_PERSONAL_SECRET", origin: :startup)
      Livebook.Secrets.set_startup_secrets([secret])

      assert secret in Provider.get_secrets(hub)
    end

    test "create_secret/1", %{hub: hub} do
      secret = build(:secret, name: "CREATE_PERSONAL_SECRET", origin: {:hub, "personal-hub"})
      assert Provider.create_secret(hub, secret) == :ok
    end

    test "update_secret/1", %{hub: hub} do
      secret = insert_secret(name: "UPDATE_PERSONAL_SECRET", origin: {:hub, "personal-hub"})
      assert secret in Secrets.get_secrets()

      updated_secret = %{secret | value: "123321"}

      assert Provider.update_secret(hub, updated_secret) == :ok
      assert updated_secret in Secrets.get_secrets()
      refute secret in Secrets.get_secrets()
    end

    test "delete_secret/1", %{hub: hub} do
      secret = insert_secret(name: "DELETE_PERSONAL_SECRET", origin: {:hub, "personal-hub"})
      assert secret in Secrets.get_secrets()

      assert Provider.delete_secret(hub, secret) == :ok
      refute secret in Secrets.get_secrets()
    end

    test "connection_error/1", %{hub: hub} do
      refute Provider.connection_error(hub)
    end
  end
end

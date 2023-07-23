defmodule Livebook.Hubs.ProviderTest do
  use Livebook.DataCase

  alias Livebook.Hubs.Provider

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
      assert_raise RuntimeError, "not implemented", fn -> Provider.disconnect(hub) end
    end

    test "capabilities/1", %{hub: hub} do
      assert Provider.capabilities(hub) == [:list_secrets, :create_secret]
    end

    test "get_secrets/1 without startup secrets", %{hub: hub} do
      secret = insert_secret(name: "GET_PERSONAL_SECRET")
      assert secret in Provider.get_secrets(hub)
    end

    test "create_secret/1", %{hub: hub} do
      secret = build(:secret, name: "CREATE_PERSONAL_SECRET")

      assert Provider.create_secret(hub, secret) == :ok
      assert secret in Provider.get_secrets(hub)
    end

    test "update_secret/1", %{hub: hub} do
      secret = insert_secret(name: "UPDATE_PERSONAL_SECRET")
      assert secret in Provider.get_secrets(hub)

      updated_secret = %{secret | value: "123321"}

      assert Provider.update_secret(hub, updated_secret) == :ok
      assert updated_secret in Provider.get_secrets(hub)
      refute secret in Provider.get_secrets(hub)
    end

    test "delete_secret/1", %{hub: hub} do
      secret = insert_secret(name: "DELETE_PERSONAL_SECRET")
      assert secret in Provider.get_secrets(hub)

      assert Provider.delete_secret(hub, secret) == :ok
      refute secret in Provider.get_secrets(hub)
    end

    test "connection_error/1", %{hub: hub} do
      assert_raise RuntimeError, "not implemented", fn -> Provider.connection_error(hub) end
    end
  end
end

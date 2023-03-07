defmodule Livebook.SecretsTest do
  use ExUnit.Case
  use Livebook.DataCase

  alias Livebook.Secrets
  alias Livebook.Secrets.Secret

  setup do
    {:ok, hub: build(:personal)}
  end

  test "get_secrets/1 returns a list of secrets from storage", %{hub: hub} do
    secret = build(:secret, name: "FOO", value: "111")

    Secrets.set_secret(secret)
    assert secret in Secrets.get_secrets(hub)

    Secrets.unset_secret(hub, secret.name)
    refute secret in Secrets.get_secrets(hub)
  end

  test "fetch an specific secret", %{hub: hub} do
    secret = insert_secret(name: "FOO", value: "111")

    assert_raise Livebook.Storage.NotFoundError,
                 ~s(could not find entry in "hub_secrets" with ID "NOT_HERE"),
                 fn ->
                   Secrets.fetch_secret!(hub, "NOT_HERE")
                 end

    assert Secrets.fetch_secret!(hub, secret.name) == secret
    Secrets.unset_secret(hub, secret.name)
  end

  describe "update_secret/2" do
    test "returns a valid secret" do
      attrs = params_for(:secret, name: "FOO", value: "111")

      assert {:ok, secret} = Secrets.update_secret(%Secret{}, attrs)
      assert attrs.name == secret.name
      assert attrs.value == secret.value
      assert attrs.hub_id == secret.hub_id
      refute secret.readonly
    end

    test "returns changeset error" do
      attrs = params_for(:secret, name: nil, value: "111")
      assert {:error, changeset} = Secrets.update_secret(%Secret{}, attrs)
      assert "can't be blank" in errors_on(changeset).name

      attrs = params_for(:secret, name: "@inavalid", value: "111")
      assert {:error, changeset} = Secrets.update_secret(%Secret{}, attrs)

      assert "should contain only alphanumeric characters and underscore" in errors_on(changeset).name
    end
  end
end

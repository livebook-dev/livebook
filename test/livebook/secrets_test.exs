defmodule Livebook.SecretsTest do
  use ExUnit.Case
  use Livebook.DataCase

  alias Livebook.Secrets

  describe "fetch_secrets/0" do
    test "returns a list of secrets from storage" do
      secret = build(:secret, name: "FOO", value: "111")

      Secrets.set_secret(secret)
      assert secret in Secrets.fetch_secrets()

      Secrets.unset_secret(secret.name)
      refute secret in Secrets.fetch_secrets()
    end

    test "returns a list of secrets from temporary storage" do
      secret = build(:secret, name: "FOO", value: "222", origin: :startup)

      Secrets.set_temporary_secrets([secret])
      assert secret in Secrets.fetch_secrets()

      # We can't delete from temporary storage, since it will be deleted
      # on next startup, if not provided
      Secrets.unset_secret(secret.name)
      assert secret in Secrets.fetch_secrets()
    end
  end

  test "fetch an specific secret" do
    secret = insert_secret(name: "FOO", value: "111")

    assert_raise Livebook.Storage.NotFoundError,
                 ~s(could not find entry in \"secrets\" with ID "NOT_HERE"),
                 fn ->
                   Secrets.fetch_secret!("NOT_HERE")
                 end

    assert Secrets.fetch_secret!(secret.name) == secret
    Secrets.unset_secret(secret.name)
  end

  test "secret_exists?/1" do
    Secrets.unset_secret("FOO")
    refute Secrets.secret_exists?("FOO")

    insert_secret(name: "FOO", value: "111")

    assert Secrets.secret_exists?("FOO")
    Secrets.unset_secret("FOO")
  end

  describe "validate_secret/1" do
    test "returns a valid secret" do
      attrs = params_for(:secret, name: "FOO", value: "111")

      assert {:ok, secret} = Secrets.validate_secret(attrs)
      assert attrs.name == secret.name
      assert attrs.value == secret.value
      assert attrs.origin == secret.origin
    end

    test "returns changeset error" do
      attrs = params_for(:secret, name: nil, value: "111")
      assert {:error, changeset} = Secrets.validate_secret(attrs)
      assert "can't be blank" in errors_on(changeset).name

      attrs = params_for(:secret, name: "@inavalid", value: "111")
      assert {:error, changeset} = Secrets.validate_secret(attrs)

      assert "should contain only alphanumeric characters and underscore" in errors_on(changeset).name
    end
  end
end

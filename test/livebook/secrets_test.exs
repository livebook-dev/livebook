defmodule Livebook.SecretsTest do
  use ExUnit.Case
  use Livebook.DataCase

  alias Livebook.Secrets
  alias Livebook.Secrets.Secret
  alias Livebook.Secrets.TemporaryStorage

  describe "fetch_secrets/0" do
    test "returns a list of secrets from storage" do
      secret = %Secret{name: "FOO", value: "111"}

      Secrets.set_secret(secret)
      assert secret in Secrets.fetch_secrets()

      Secrets.unset_secret(secret.name)
      refute secret in Secrets.fetch_secrets()
    end

    test "returns a list of secrets from temporary storage" do
      secret = %Secret{name: "BAR", value: "222"}

      TemporaryStorage.set_secret(secret)
      assert secret in Secrets.fetch_secrets()

      # We can't delete from temporary storage, since it will be deleted
      # on next startup, if not provided
      Secrets.unset_secret(secret.name)
      assert secret in Secrets.fetch_secrets()
    end
  end

  test "fetch an specific secret" do
    secret = %Secret{name: "FOO", value: "111"}
    Secrets.set_secret(secret)

    assert_raise Livebook.Storage.NotFoundError,
                 ~s(could not find entry in \"secrets\" with ID "NOT_HERE"),
                 fn ->
                   Secrets.fetch_secret!("NOT_HERE")
                 end

    assert Secrets.fetch_secret!(secret.name) == %Secret{name: "FOO", value: "111"}
    Secrets.unset_secret(secret.name)
  end

  test "secret_exists?/1" do
    Secrets.unset_secret("FOO")
    refute Secrets.secret_exists?("FOO")
    Secrets.set_secret(%Secret{name: "FOO", value: "111"})
    assert Secrets.secret_exists?("FOO")
    Secrets.unset_secret("FOO")
  end

  describe "validate_secret/1" do
    test "returns a valid secret" do
      attrs = %{name: "FOO", value: "111"}
      assert {:ok, secret} = Secrets.validate_secret(attrs)
      assert attrs.name == secret.name
      assert attrs.value == secret.value
    end

    test "returns changeset error" do
      attrs = %{value: "111"}
      assert {:error, changeset} = Secrets.validate_secret(attrs)
      assert "can't be blank" in errors_on(changeset).name
      attrs = %{name: "@inavalid", value: "111"}

      assert {:error, changeset} = Secrets.validate_secret(attrs)

      assert "should contain only alphanumeric characters and underscore" in errors_on(changeset).name
    end
  end
end

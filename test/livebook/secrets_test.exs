defmodule Livebook.SecretsTest do
  use ExUnit.Case
  use Livebook.DataCase

  alias Livebook.Secrets
  alias Livebook.Secrets.Secret

  test "fetch secrets" do
    Secrets.set_secret(%{name: "FOO", value: "111"})
    assert %Secret{name: "FOO", value: "111"} in Secrets.fetch_secrets()

    Secrets.unset_secret("FOO")
    refute %Secret{name: "FOO", value: "111"} in Secrets.fetch_secrets()
  end

  test "fetch an specif secret" do
    secret = %{name: "FOO", value: "111"}
    Secrets.set_secret(secret)

    assert_raise Secrets.NotFoundError,
                 ~s(could not find the secret matching "NOT_HERE"),
                 fn ->
                   Secrets.fetch_secret!("NOT_HERE")
                 end

    assert Secrets.fetch_secret!(secret.name) == %Secret{name: "FOO", value: "111"}
    Secrets.unset_secret(secret.name)
  end

  test "secret_exists?/1" do
    refute Secrets.secret_exists?("FOO")
    Secrets.set_secret(%{name: "FOO", value: "111"})
    assert Secrets.secret_exists?("FOO")
    Secrets.unset_secret("FOO")
  end

  describe "set_secret/1" do
    test "creates and stores a secret" do
      attrs = %{name: "FOO", value: "111"}
      assert {:ok, secret} = Secrets.set_secret(attrs)

      assert attrs.name == secret.name
      assert attrs.value == secret.value

      Secrets.unset_secret(secret.name)
    end

    test "updates an stored secret" do
      secret = %Secret{name: "FOO", value: "111"}
      attrs = %{value: "222"}
      assert {:ok, updated_secret} = Secrets.set_secret(secret, attrs)

      assert secret.name == updated_secret.name
      assert updated_secret.value == attrs.value

      Secrets.unset_secret(secret.name)
    end

    test "returns changeset error" do
      attrs = %{value: "111"}
      assert {:error, changeset} = Secrets.set_secret(attrs)
      assert "can't be blank" in errors_on(changeset).name
      attrs = %{name: "@inavalid", value: "111"}

      assert {:error, changeset} = Secrets.set_secret(attrs)
      assert "should contain only alphanumeric and underscore" in errors_on(changeset).name
    end
  end
end

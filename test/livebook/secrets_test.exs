defmodule Livebook.SecretsTest do
  use Livebook.DataCase, async: true

  alias Livebook.Secrets
  alias Livebook.Secrets.Secret

  describe "update_secret/2" do
    test "returns a valid secret" do
      attrs = params_for(:secret, name: "FOO", value: "111")

      assert {:ok, secret} = Secrets.update_secret(%Secret{}, attrs)
      assert attrs.name == secret.name
      assert attrs.value == secret.value
      assert attrs.hub_id == secret.hub_id
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

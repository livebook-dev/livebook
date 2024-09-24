defmodule Livebook.Hubs.PersonalTest do
  use Livebook.DataCase

  alias Livebook.Hubs.Personal

  test "get_secrets/1 returns a list of secrets from storage" do
    secret = build(:secret, name: "FOO", value: "111")

    Personal.set_secret(secret)
    assert secret in Personal.get_secrets()

    Personal.unset_secret(secret.name)
    refute secret in Personal.get_secrets()
  end

  test "fetch an specific secret" do
    secret = insert_secret(name: "FOO", value: "111")

    assert_raise Livebook.Storage.NotFoundError,
                 ~s(could not find entry in "hub_secrets" with ID "NOT_HERE"),
                 fn ->
                   Personal.fetch_secret!("NOT_HERE")
                 end

    assert Personal.fetch_secret!(secret.name) == secret
    Personal.unset_secret(secret.name)
  end
end

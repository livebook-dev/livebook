defmodule Livebook.SettingsTest do
  use Livebook.DataCase

  alias Livebook.Settings

  test "fetch_env_vars/0 returns a list of persisted environment variables" do
    env_var = insert_env_var(:env_var)
    assert env_var in Settings.fetch_env_vars()

    Settings.unset_env_var(env_var.name)
    refute env_var in Settings.fetch_env_vars()
  end

  test "fetch_env_var!/1 returns one persisted environment variable" do
    %{name: name} = build(:env_var)

    assert_raise Livebook.Storage.NotFoundError,
                 ~s/could not find entry in \"env_vars\" with ID "#{name}"/,
                 fn ->
                   Settings.fetch_env_var!(name)
                 end

    env_var = insert_env_var(:env_var, name: name)
    assert Settings.fetch_env_var!(name) == env_var

    Settings.unset_env_var(name)
  end

  test "env_var_exists?/1" do
    %{name: name} = build(:env_var)

    refute Settings.env_var_exists?(name)
    insert_env_var(:env_var, name: name)
    assert Settings.env_var_exists?(name)

    Settings.unset_env_var(name)
  end

  describe "set_env_var/1" do
    test "creates an environment variable" do
      attrs = params_for(:env_var)
      assert {:ok, env_var} = Settings.set_env_var(attrs)

      assert attrs.name == env_var.name
      assert attrs.value == env_var.value

      Settings.unset_env_var(env_var.name)
    end

    test "updates an environment variable" do
      env_var = insert_env_var(:env_var)
      attrs = %{value: "FOO"}
      assert {:ok, updated_env_var} = Settings.set_env_var(env_var, attrs)

      assert env_var.name == updated_env_var.name
      assert updated_env_var.value == attrs.value

      Settings.unset_env_var(env_var.name)
    end

    test "returns changeset error" do
      attrs = params_for(:env_var, name: nil)
      assert {:error, changeset} = Settings.set_env_var(attrs)
      assert "can't be blank" in errors_on(changeset).name

      assert {:error, changeset} = Settings.set_env_var(%{attrs | name: "LB_FOO"})
      assert "cannot start with the LB_ prefix" in errors_on(changeset).name
    end
  end
end

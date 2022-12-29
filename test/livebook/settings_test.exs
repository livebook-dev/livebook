defmodule Livebook.SettingsTest do
  use Livebook.DataCase

  alias Livebook.Settings

  test "fetch_env_vars/0 returns a list of persisted environment variables" do
    env_var = insert_env_var(:env_var)
    assert env_var in Settings.fetch_env_vars()

    Settings.unset_env_var(env_var.name)
    refute env_var in Settings.fetch_env_vars()
  end

  test "fetch_env_var!/1 returns one persisted fly" do
    assert_raise Livebook.Storage.NotFoundError,
                 ~s/could not find entry in \"env_vars\" with ID "123456"/,
                 fn ->
                   Settings.fetch_env_var!("123456")
                 end

    env_var = insert_env_var(:env_var, name: "123456")
    assert Settings.fetch_env_var!("123456") == env_var

    Settings.unset_env_var("123456")
  end

  test "env_var_exists?/1" do
    refute Settings.env_var_exists?("FOO")
    insert_env_var(:env_var, name: "FOO")
    assert Settings.env_var_exists?("FOO")

    Settings.unset_env_var("FOO")
  end

  describe "set_env_var/1" do
    test "creates an environment variable" do
      attrs = params_for(:env_var, name: "FOO_BAR_BAZ")
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

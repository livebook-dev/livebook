defmodule Livebook.SettingsTest do
  use Livebook.DataCase

  alias Livebook.Settings

  test "fetch_env_vars/0 returns a list of persisted environment variables" do
    env_var = insert_env_var(:env_var)
    assert env_var in Settings.fetch_env_vars()

    Settings.delete_env_var(env_var.key)
    refute env_var in Settings.fetch_env_vars()
  end

  test "fetch_env_var!/1 returns one persisted fly" do
    assert_raise Settings.NotFoundError,
                 ~s/could not find an environment variable matching "123456"/,
                 fn ->
                   Settings.fetch_env_var!("123456")
                 end

    env_var = insert_env_var(:env_var, key: "123456")
    assert Settings.fetch_env_var!("123456") == env_var

    Settings.delete_env_var("123456")
  end

  test "env_var_exists?/1" do
    refute Settings.env_var_exists?("FOO")
    insert_env_var(:env_var, key: "FOO")
    assert Settings.env_var_exists?("FOO")

    Settings.delete_env_var("FOO")
  end

  describe "set_env_var/1" do
    test "creates an environment variable" do
      attrs = params_for(:env_var, key: "FOO_BAR_BAZ")
      assert {:ok, env_var} = Settings.set_env_var(attrs)

      assert attrs.key == env_var.key
      assert attrs.value == env_var.value

      Settings.delete_env_var(env_var.key)
    end

    test "updates an environment variable" do
      env_var = insert_env_var(:env_var)
      attrs = %{value: "FOO"}
      assert {:ok, updated_env_var} = Settings.set_env_var(env_var, attrs)

      assert env_var.key == updated_env_var.key
      assert updated_env_var.value == attrs.value

      Settings.delete_env_var(env_var.key)
    end

    test "returns changeset error" do
      attrs = params_for(:env_var, key: nil)
      assert {:error, changeset} = Settings.set_env_var(attrs)
      assert "can't be blank" in errors_on(changeset).key

      assert {:error, changeset} = Settings.set_env_var(%{attrs | key: "LB_FOO"})
      assert "cannot start with the LB_ prefix" in errors_on(changeset).key
    end
  end
end

defmodule Livebook.SettingsTest do
  use Livebook.DataCase

  alias Livebook.Settings

  test "fetch_env_vars/0 returns a list of persisted environment variables" do
    env_var = insert_env_var(:environment_variable)
    assert env_var in Settings.fetch_env_vars()

    Settings.delete_env_var(env_var.id)
    refute env_var in Settings.fetch_env_vars()
  end

  test "fetch_env_var!/1 returns one persisted fly" do
    assert_raise RuntimeError,
                 ~s/the environment variable 123456 does not exists in storage/,
                 fn ->
                   Settings.fetch_env_var!("123456")
                 end

    env_var = insert_env_var(:environment_variable, id: "123456")
    assert Settings.fetch_env_var!("123456") == env_var

    Settings.delete_env_var("123456")
  end

  test "env_var_exists?/1" do
    refute Settings.env_var_exists?("FOO")
    insert_env_var(:environment_variable, key: "FOO")
    assert Settings.env_var_exists?("FOO")

    Settings.delete_env_var("FOO")
  end

  describe "create_env_var/1" do
    test "creates an environment variable" do
      attrs = params_for(:environment_variable, key: "FOO_BAR_BAZ")
      assert {:ok, env_var} = Settings.create_env_var(attrs)

      assert attrs.key == env_var.key
      assert attrs.value == env_var.value

      Settings.delete_env_var(env_var.id)
    end

    test "returns changeset error" do
      attrs = params_for(:environment_variable, key: nil)
      assert {:error, changeset} = Settings.create_env_var(attrs)
      assert "can't be blank" in errors_on(changeset).key

      assert {:error, changeset} = Settings.create_env_var(%{attrs | key: "LB_FOO"})
      assert "has invalid format" in errors_on(changeset).key

      attrs = params_for(:environment_variable, key: "JAKE_PERALTA")
      assert {:ok, env_var} = Settings.create_env_var(attrs)
      assert {:error, changeset} = Settings.create_env_var(attrs)
      assert "already exists" in errors_on(changeset).key

      Settings.delete_env_var(env_var.id)
    end
  end

  describe "update_env_var/2" do
    test "updates an environment variable" do
      env_var = insert_env_var(:environment_variable)
      attrs = %{value: "FOO"}
      assert {:ok, updated_env_var} = Settings.update_env_var(env_var, attrs)

      assert env_var.key == updated_env_var.key
      assert updated_env_var.value == attrs.value

      Settings.delete_env_var(env_var.id)
    end

    test "returns changeset error" do
      env_var = insert_env_var(:environment_variable)
      attrs = %{key: nil}
      assert {:error, changeset} = Settings.update_env_var(env_var, attrs)
      assert "can't be blank" in errors_on(changeset).key

      assert {:error, changeset} = Settings.update_env_var(env_var, %{attrs | key: "LB_FOO"})
      assert "has invalid format" in errors_on(changeset).key

      env_var = build(:environment_variable, key: "DEF")
      attrs = %{}
      assert {:error, changeset} = Settings.update_env_var(env_var, attrs)
      assert "does not exists" in errors_on(changeset).key
    end
  end
end

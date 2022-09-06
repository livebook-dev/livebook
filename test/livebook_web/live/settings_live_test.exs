defmodule LivebookWeb.SettingsLiveTest do
  use LivebookWeb.ConnCase, async: true
  @moduletag :tmp_dir

  import Phoenix.LiveViewTest

  alias Livebook.Settings

  setup %{tmp_dir: tmp_dir} do
    Livebook.Storage.current().insert(:settings, "global", autosave_path: tmp_dir)

    on_exit(fn ->
      Livebook.Storage.current().insert(:settings, "global", autosave_path: nil)
    end)

    :ok
  end

  describe "environment variables configuration" do
    test "list persisted environment variables", %{conn: conn} do
      insert_env_var(:environment_variable, key: "MY_ENVIRONMENT_VAR")
      {:ok, _view, html} = live(conn, Routes.settings_path(conn, :page))

      assert html =~ "MY_ENVIRONMENT_VAR"
    end

    test "adds an environment variable", %{conn: conn} do
      attrs = params_for(:environment_variable)

      {:ok, view, html} = live(conn, Routes.settings_path(conn, :add_env_var))

      refute html =~ attrs.key

      view
      |> element("#add-env-var-form")
      |> render_submit(%{"environment_variable" => attrs})

      assert_patch(view, Routes.settings_path(conn, :page))

      assert render(view) =~ attrs.key
    end

    test "updates an environment variable", %{conn: conn} do
      env_var = insert_env_var(:environment_variable)
      {:ok, view, html} = live(conn, Routes.settings_path(conn, :page))

      assert html =~ env_var.key

      view
      |> with_target("#env-vars")
      |> render_click("edit_env_var", %{"env_var" => env_var.id})

      assert_patch(view, Routes.settings_path(conn, :edit_env_var))

      form = element(view, "#edit-env-var-form")
      assert render(form) =~ env_var.value

      render_submit(form, %{"environment_variable" => %{"value" => "123456"}})
      assert_patch(view, Routes.settings_path(conn, :page))

      updated_env_var = Settings.fetch_env_var!(env_var.id)

      assert updated_env_var.key == env_var.key
      refute updated_env_var.value == env_var.value
    end

    test "deletes an environment variable", %{conn: conn} do
      env_var = insert_env_var(:environment_variable)
      {:ok, view, html} = live(conn, Routes.settings_path(conn, :page))

      assert html =~ env_var.key

      view
      |> with_target("#env-vars")
      |> render_click("delete_env_var", %{"env_var" => env_var.id})

      refute render(view) =~ env_var.key
    end
  end
end

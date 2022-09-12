defmodule LivebookWeb.SettingsLiveTest do
  use LivebookWeb.ConnCase, async: true
  @moduletag :tmp_dir

  import Phoenix.LiveViewTest

  alias Livebook.Settings

  describe "environment variables configuration" do
    test "list persisted environment variables", %{conn: conn} do
      insert_env_var(:env_var, key: "MY_ENVIRONMENT_VAR")
      {:ok, _view, html} = live(conn, Routes.settings_path(conn, :page))

      assert html =~ "MY_ENVIRONMENT_VAR"
    end

    test "adds an environment variable", %{conn: conn} do
      attrs = params_for(:env_var, key: "JAKE_PERALTA_ENV_VAR")

      {:ok, view, html} = live(conn, Routes.settings_path(conn, :add_env_var))

      assert html =~ "Add environment variable"
      refute html =~ attrs.key

      view
      |> element("#env-var-form")
      |> render_change(%{"env_var" => attrs})

      refute view
             |> element("#env-var-form .invalid-feedback")
             |> has_element?()

      view
      |> element("#env-var-form")
      |> render_submit(%{"env_var" => attrs})

      assert_patch(view, Routes.settings_path(conn, :page))

      assert render(view) =~ attrs.key
    end

    test "updates an environment variable", %{conn: conn} do
      env_var = insert_env_var(:env_var, key: "UPDATE_ME")

      {:ok, view, html} = live(conn, Routes.settings_path(conn, :page))

      assert html =~ env_var.key

      view
      |> with_target("#env-vars")
      |> render_click("edit_env_var", %{"env_var" => env_var.key})

      assert_patch(view, Routes.settings_path(conn, :edit_env_var, env_var.key))
      assert render(view) =~ "Edit environment variable"

      form = element(view, "#env-var-form")
      assert render(form) =~ env_var.value

      render_change(form, %{"env_var" => %{"value" => "123456"}})

      refute view
             |> element(".invalid-feedback")
             |> has_element?()

      render_submit(form, %{"env_var" => %{"value" => "123456"}})
      assert_patch(view, Routes.settings_path(conn, :page))

      updated_env_var = Settings.fetch_env_var!(env_var.key)

      assert updated_env_var.key == env_var.key
      refute updated_env_var.value == env_var.value
    end

    test "deletes an environment variable", %{conn: conn} do
      env_var = insert_env_var(:env_var)
      {:ok, view, html} = live(conn, Routes.settings_path(conn, :page))

      assert html =~ env_var.key

      view
      |> with_target("#env-vars")
      |> render_click("delete_env_var", %{"env_var" => env_var.key})

      refute render(view) =~ env_var.key
    end
  end
end

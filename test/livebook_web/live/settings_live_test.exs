defmodule LivebookWeb.SettingsLiveTest do
  use LivebookWeb.ConnCase, async: true
  @moduletag :tmp_dir

  import Phoenix.LiveViewTest
  import Livebook.TestHelpers

  alias Livebook.Settings

  describe "environment variables configuration" do
    test "list persisted environment variables", %{conn: conn} do
      insert_env_var(:env_var, name: "MY_ENVIRONMENT_VAR")
      assert conn |> get(~p"/settings") |> html_response(200) =~ "MY_ENVIRONMENT_VAR"
    end

    test "adds an environment variable", %{conn: conn} do
      attrs = params_for(:env_var)

      {:ok, view, html} = live(conn, ~p"/settings/env-var/new")

      assert html =~ "Add environment variable"
      refute html =~ attrs.name

      view
      |> element("#env-var-form")
      |> render_change(%{"env_var" => attrs})

      refute view
             |> element("#env-var-form .invalid-feedback")
             |> has_element?()

      view
      |> element("#env-var-form")
      |> render_submit(%{"env_var" => attrs})

      assert_patch(view, ~p"/settings")

      assert render(view) =~ attrs.name
    end

    test "updates an environment variable", %{conn: conn} do
      env_var = insert_env_var(:env_var, name: "UPDATE_ME")

      {:ok, view, html} = live(conn, ~p"/settings")

      assert html =~ env_var.name

      render_click(view, "edit_env_var", %{"env_var" => env_var.name})

      assert_patch(view, ~p"/settings/env-var/edit/#{env_var.name}")
      assert render(view) =~ "Edit environment variable"

      form = element(view, "#env-var-form")
      assert render(form) =~ env_var.value

      render_change(form, %{"env_var" => %{"value" => "123456"}})

      refute view
             |> element(".invalid-feedback")
             |> has_element?()

      render_submit(form, %{"env_var" => %{"value" => "123456"}})
      assert_patch(view, ~p"/settings")

      updated_env_var = Settings.fetch_env_var!(env_var.name)

      assert updated_env_var.name == env_var.name
      refute updated_env_var.value == env_var.value
    end

    test "deletes an environment variable", %{conn: conn} do
      env_var = insert_env_var(:env_var)
      {:ok, view, html} = live(conn, ~p"/settings")

      assert html =~ env_var.name

      view
      |> element("#env-var-#{env_var.name}-delete")
      |> render_click()

      render_confirm(view)

      refute view
             |> element("#env-vars")
             |> render() =~ env_var.name
    end
  end

  describe "autosaving" do
    # Autosaving can only be disabled by directly changing storage,
    # which we do for tests. That said, it is a valid setting, so we
    # have a UI to enable autosaving back, just in case someone boots
    # Livebook with such storage.
    test "can be enabled", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/settings")

      view
      |> element("#enable-autosave")
      |> render_click()

      view
      |> element("#cancel-autosave")
      |> render_click()
    end
  end
end

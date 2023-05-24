defmodule LivebookWeb.AppAuthLiveTest do
  use LivebookWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  setup ctx do
    {slug, app_pid} = create_app(ctx[:app_settings] || %{})

    on_exit(fn ->
      Livebook.App.close(app_pid)
    end)

    Application.put_env(:livebook, :authentication_mode, :password)
    Application.put_env(:livebook, :password, ctx[:livebook_password])

    on_exit(fn ->
      Application.put_env(:livebook, :authentication_mode, :disabled)
      Application.delete_env(:livebook, :password)
    end)

    %{slug: slug}
  end

  defp create_app(app_settings_attrs) do
    slug = Livebook.Utils.random_id()

    app_settings =
      Livebook.Notebook.AppSettings.new()
      |> Map.replace!(:slug, slug)
      |> Map.merge(app_settings_attrs)

    notebook = %{Livebook.Notebook.new() | app_settings: app_settings}

    {:ok, app_pid} = Livebook.Apps.deploy(notebook)

    {slug, app_pid}
  end

  # Integration tests for the authentication scenarios

  describe "public app" do
    @describetag app_settings: %{access_type: :public}

    test "does not require authentication", %{conn: conn, slug: slug} do
      {:ok, view, _} =
        conn
        |> live(~p"/apps/#{slug}")
        |> follow_redirect(conn)

      assert render(view) =~ "Untitled notebook"
    end
  end

  describe "protected app" do
    @describetag livebook_password: "long_livebook_password"
    @describetag app_settings: %{access_type: :protected, password: "long_app_password"}

    test "redirect to auth page when not authenticated", %{conn: conn, slug: slug} do
      {:error, {:live_redirect, %{to: to}}} = live(conn, ~p"/apps/#{slug}")
      assert to == "/apps/#{slug}/authenticate"
    end

    test "shows an error on invalid password", %{conn: conn, slug: slug} do
      {:ok, view, _} = live(conn, ~p"/apps/#{slug}/authenticate")

      assert view
             |> element("form")
             |> render_submit(%{password: "invalid password"}) =~ "app password is invalid"
    end

    test "persists authentication across requests", %{conn: conn, slug: slug} do
      {:ok, view, _} = live(conn, ~p"/apps/#{slug}/authenticate")

      view
      |> element("form")
      |> render_submit(%{password: "long_app_password"})

      # The token is stored on the client

      assert_push_event(view, "persist_app_auth", %{"slug" => ^slug, "token" => token})

      assert {:error, {:live_redirect, %{to: to}}} = render_hook(view, "app_auth_persisted")
      assert to == "/apps/#{slug}"

      # Then, the client passes the token in connect params

      conn = put_connect_params(conn, %{"app_auth_token" => token})

      {:ok, view, _} =
        conn
        |> live("/apps/#{slug}")
        |> follow_redirect(conn)

      assert render(view) =~ "Untitled notebook"

      # The auth page redirects to the app

      {:error, {:live_redirect, %{to: to}}} = live(conn, "/apps/#{slug}/authenticate")

      assert to == "/apps/#{slug}"
    end

    test "when redirected from app session page, returns to that same page",
         %{conn: conn, slug: slug} do
      {:ok, %{sessions: [%{id: session_id}]}} = Livebook.Apps.fetch_app(slug)

      # We navigate to a specific session and get redirected to auth

      {:ok, view, _} =
        conn
        |> live(~p"/apps/#{slug}/#{session_id}")
        |> follow_redirect(conn)

      view
      |> element("form")
      |> render_submit(%{password: "long_app_password"})

      assert_push_event(view, "persist_app_auth", %{"slug" => ^slug, "token" => _token})

      assert {:error, {:live_redirect, %{to: to}}} = render_hook(view, "app_auth_persisted")
      assert to == ~p"/apps/#{slug}/#{session_id}"
    end

    test "redirects to the app page when authenticating in Livebook", %{conn: conn, slug: slug} do
      conn = get(conn, ~p"/authenticate?redirect_to=/apps/#{slug}")
      assert redirected_to(conn) == "/authenticate"

      conn = post(conn, ~p"/authenticate", password: "long_livebook_password")
      assert redirected_to(conn) == "/apps/#{slug}"

      {:ok, view, _} =
        conn
        |> live(~p"/apps/#{slug}")
        |> follow_redirect(conn)

      assert render(view) =~ "Untitled notebook"

      # The auth page redirects to the app

      {:error, {:live_redirect, %{to: to}}} = live(conn, ~p"/apps/#{slug}/authenticate")
      assert to == "/apps/#{slug}"
    end
  end

  test "redirects to homepage when accessing non-existent app", %{conn: conn} do
    assert {:error, {:redirect, %{to: "/"}}} = live(conn, ~p"/apps/nonexistent")
  end
end

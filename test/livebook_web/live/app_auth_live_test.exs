defmodule LivebookWeb.AppAuthLiveTest do
  use LivebookWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  setup ctx do
    {slug, session} = create_app(ctx[:app_settings] || %{})

    on_exit(fn ->
      Livebook.Session.close(session.pid)
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

    {:ok, session} = Livebook.Sessions.create_session(notebook: notebook, mode: :app)
    Livebook.Session.app_subscribe(session.id)
    Livebook.Session.app_build(session.pid)

    session_id = session.id
    assert_receive {:app_registration_changed, ^session_id, true}

    {slug, session}
  end

  # Integration tests for the authentication scenarios

  describe "public app" do
    @describetag app_settings: %{access_type: :public}

    test "does not require authentication", %{conn: conn, slug: slug} do
      {:ok, view, _} = live(conn, "/apps/#{slug}")
      assert render(view) =~ "Untitled notebook"
    end
  end

  describe "protected app" do
    @describetag livebook_password: "long_livebook_password"
    @describetag app_settings: %{access_type: :protected, password: "long_app_password"}

    test "redirect to auth page when not authenticated", %{conn: conn, slug: slug} do
      {:error, {:live_redirect, %{to: to}}} = live(conn, "/apps/#{slug}")
      assert to == "/apps/#{slug}/authenticate"
    end

    test "shows an error on invalid password", %{conn: conn, slug: slug} do
      {:ok, view, _} = live(conn, "/apps/#{slug}/authenticate")

      assert view
             |> element("form")
             |> render_submit(%{password: "invalid password"}) =~ "app password is invalid"
    end

    test "persists authentication across requests", %{conn: conn, slug: slug} do
      {:ok, view, _} = live(conn, "/apps/#{slug}/authenticate")

      view
      |> element("form")
      |> render_submit(%{password: "long_app_password"})

      # The token is stored on the client

      assert_push_event(view, "persist_app_auth", %{"slug" => ^slug, "token" => token})

      assert {:error, {:live_redirect, %{to: to}}} = render_hook(view, "app_auth_persisted")
      assert to == "/apps/#{slug}"

      # Then, the client passes the token in connect params

      {:ok, view, _} =
        conn
        |> put_connect_params(%{"app_auth_token" => token})
        |> live("/apps/#{slug}")

      assert render(view) =~ "Untitled notebook"

      # The auth page redirects to the app

      {:error, {:live_redirect, %{to: to}}} =
        conn
        |> put_connect_params(%{"app_auth_token" => token})
        |> live("/apps/#{slug}/authenticate")

      assert to == "/apps/#{slug}"
    end

    test "redirects to the app page when authenticating in Livebook", %{conn: conn, slug: slug} do
      conn = get(conn, "/authenticate?redirect_to=/apps/#{slug}")
      assert redirected_to(conn) == "/authenticate"

      conn = post(conn, "/authenticate", password: "long_livebook_password")
      assert redirected_to(conn) == "/apps/#{slug}"

      {:ok, view, _} = live(conn, "/apps/#{slug}")
      assert render(view) =~ "Untitled notebook"

      # The auth page redirects to the app

      {:error, {:live_redirect, %{to: to}}} = live(conn, "/apps/#{slug}/authenticate")
      assert to == "/apps/#{slug}"
    end
  end

  test "redirects to homepage when accessing non-existent app", %{conn: conn} do
    assert {:error, {:redirect, %{to: "/"}}} = live(conn, "/apps/nonexistent")
  end
end

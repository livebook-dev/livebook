defmodule LivebookWeb.AppAuthPlugTest do
  use LivebookWeb.ConnCase, async: false

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

  describe "public app" do
    @describetag app_settings: %{access_type: :public}

    test "does not require authentication", %{conn: conn, slug: slug} do
      conn = get(conn, "/apps/#{slug}")
      assert conn.status == 200
      assert conn.resp_body =~ "Untitled notebook"
    end
  end

  describe "protected app" do
    @describetag livebook_password: "long_livebook_password"
    @describetag app_settings: %{access_type: :protected, password: "long_app_password"}

    test "redirect to auth page when not authenticated", %{conn: conn, slug: slug} do
      conn = get(conn, "/apps/#{slug}")
      assert redirected_to(conn) == "/apps/#{slug}/authenticate"
    end

    test "redirects back to auth on invalid password", %{conn: conn, slug: slug} do
      conn = post(conn, "/apps/#{slug}/authenticate", password: "invalid password")
      assert html_response(conn, 200) =~ "Authentication required"

      conn = get(conn, "/apps/#{slug}")
      assert redirected_to(conn) == "/apps/#{slug}/authenticate"
    end

    test "persists authentication across requests", %{conn: conn, slug: slug} do
      conn = post(conn, "/apps/#{slug}/authenticate", password: "long_app_password")
      assert redirected_to(conn) == "/apps/#{slug}"
      assert get_session(conn, "80:app_password:#{slug}")

      conn = get(conn, "/apps/#{slug}")
      assert conn.status == 200
      assert conn.resp_body =~ "Untitled notebook"

      conn = get(conn, "/apps/#{slug}/authenticate")
      assert redirected_to(conn) == "/apps/#{slug}"
    end

    test "redirects to the app page when authenticating in Livebook", %{conn: conn, slug: slug} do
      conn = get(conn, "/apps/#{slug}/authenticate/global")
      assert redirected_to(conn) == "/authenticate"

      conn = post(conn, "/authenticate", password: "long_livebook_password")
      assert redirected_to(conn) == "/apps/#{slug}"
    end
  end
end

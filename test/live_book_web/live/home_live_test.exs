defmodule LiveBookWeb.HomeLiveTest do
  use LiveBookWeb.ConnCase

  import Phoenix.LiveViewTest

  alias LiveBook.SessionSupervisor

  test "disconnected and connected render", %{conn: conn} do
    {:ok, view, disconnected_html} = live(conn, "/")
    assert disconnected_html =~ "LiveBook"
    assert render(view) =~ "LiveBook"
  end

  test "redirects to session upon creation", %{conn: conn} do
    {:ok, view, _} = live(conn, "/")

    assert {:error, {:live_redirect, %{to: to}}} =
             view
             |> element("button", "New notebook")
             |> render_click()

    assert to =~ "/sessions/"
  end

  describe "file selection" do
    test "updates the list of files as the input changes", %{conn: conn} do
      {:ok, view, _} = live(conn, "/")

      path = Path.expand("../../../lib", __DIR__) <> "/"

      assert view
             |> element("form")
             |> render_change(%{path: path}) =~ "live_book_web"
    end

    test "allows importing when a notebook file is selected", %{conn: conn} do
      {:ok, view, _} = live(conn, "/")

      path = test_notebook_path("basic")

      view
      |> element("form")
      |> render_change(%{path: path})

      assert assert {:error, {:live_redirect, %{to: to}}} =
                      view
                      |> element("button", "Fork")
                      |> render_click()

      assert to =~ "/sessions/"
    end

    test "disables import when a directory is selected", %{conn: conn} do
      {:ok, view, _} = live(conn, "/")

      path = File.cwd!()

      view
      |> element("form")
      |> render_change(%{path: path})

      assert view
             |> element("button[disabled]", "Fork")
             |> has_element?()
    end

    test "disables import when a nonexistent file is selected", %{conn: conn} do
      {:ok, view, _} = live(conn, "/")

      path = File.cwd!() |> Path.join("nonexistent.livemd")

      view
      |> element("form")
      |> render_change(%{path: path})

      assert view
             |> element("button[disabled]", "Fork")
             |> has_element?()
    end
  end

  describe "sessions list" do
    test "lists running sessions", %{conn: conn} do
      {:ok, id1} = SessionSupervisor.create_session()
      {:ok, id2} = SessionSupervisor.create_session()

      {:ok, view, _} = live(conn, "/")

      assert render(view) =~ id1
      assert render(view) =~ id2
    end

    test "updates UI whenever a session is added or deleted", %{conn: conn} do
      {:ok, view, _} = live(conn, "/")

      {:ok, id} = SessionSupervisor.create_session()
      assert render(view) =~ id

      SessionSupervisor.delete_session(id)
      refute render(view) =~ id
    end
  end

  # Helpers

  defp test_notebook_path(name) do
    path =
      ["../../support/notebooks", name <> ".livemd"]
      |> Path.join()
      |> Path.expand(__DIR__)

    unless File.exists?(path) do
      raise "Cannot find test notebook with the name: #{name}"
    end

    path
  end
end

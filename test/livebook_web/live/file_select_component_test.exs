defmodule LivebookWeb.FileSelectComponentTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest
  import Livebook.TestHelpers

  alias Livebook.FileSystem

  test "when the path has a trailing slash, lists that directory", %{conn: conn} do
    file = FileSystem.File.local(notebooks_path() <> "/")
    html = render_file_select(conn, file)

    assert html =~ "basic.livemd"
    assert html =~ ".."
  end

  test "when the path has no trailing slash, lists the parent directory", %{conn: conn} do
    file = FileSystem.File.local(notebooks_path())

    assert render_file_select(conn, file) =~ "notebooks"
  end

  test "does not show parent directory when in root", %{conn: conn} do
    file = FileSystem.File.local(p("/"))

    refute render_file_select(conn, file) =~ ".."
  end

  defp render_file_select(conn, file) do
    {:ok, view, _html} =
      live_isolated(conn, LivebookWeb.FileSelectComponentTest.Live,
        session: %{"path" => file.path}
      )

    render_async(view)
  end

  defp notebooks_path() do
    Path.expand("../../support/notebooks", __DIR__)
  end

  defmodule Live do
    use Phoenix.LiveView, layout: false

    alias Livebook.FileSystem
    alias LivebookWeb.FileSelectComponent

    @impl true
    def mount(_params, %{"path" => path}, socket) do
      socket =
        assign(socket,
          file: FileSystem.File.local(path),
          extnames: [".livemd"],
          running_files: []
        )

      {:ok, socket}
    end

    @impl true
    def render(assigns) do
      ~H"""
      <.live_component
        module={FileSelectComponent}
        id="1"
        file={@file}
        extnames={@extnames}
        running_files={@running_files}
      />
      """
    end
  end
end

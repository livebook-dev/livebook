defmodule LivebookWeb.FileSelectComponentTest do
  use LivebookWeb.ConnCase, async: true

  import Phoenix.LiveViewTest
  import Livebook.TestHelpers

  alias Livebook.FileSystem
  alias LivebookWeb.FileSelectComponent

  test "when the path has a trailing slash, lists that directory" do
    file = FileSystem.File.local(notebooks_path() <> "/")
    assert render_component(FileSelectComponent, attrs(file: file)) =~ "basic.livemd"
    assert render_component(FileSelectComponent, attrs(file: file)) =~ ".."
  end

  test "when the path has no trailing slash, lists the parent directory" do
    file = FileSystem.File.local(notebooks_path())
    assert render_component(FileSelectComponent, attrs(file: file)) =~ "notebooks"
  end

  test "does not show parent directory when in root" do
    file = FileSystem.File.local(p("/"))
    refute render_component(FileSelectComponent, attrs(file: file)) =~ ".."
  end

  test "shows loading when changing directories" do
    file = FileSystem.File.local(notebooks_path() <> "/")

    # Initial render in one directory
    html =
      render_component(
        FileSelectComponent,
        attrs(file: file, id: "test-loading-dir-change")
      )

    # Loading should not be shown after initial render
    refute html =~ ~s(role="status")

    # Simulate changing to a different directory via send_update with a new file
    new_file = FileSystem.File.local(p("/"))

    html =
      render_component(
        FileSelectComponent,
        attrs(file: new_file, id: "test-loading-dir-change")
      )

    # After changing directory, loading should be reset to false after listing
    refute html =~ ~s(role="status")
  end

  test "does not show loading when only running_files changes" do
    file = FileSystem.File.local(notebooks_path() <> "/")
    running_file = FileSystem.File.local(notebooks_path() <> "/basic.livemd")

    # Initial render with no running files
    html =
      render_component(
        FileSelectComponent,
        attrs(file: file, id: "test-loading-running", running_files: [])
      )

    refute html =~ ~s(role="status")
    # Verify basic.livemd is shown but not marked as running
    assert html =~ "basic.livemd"
    refute html =~ "play-circle-line"

    # Update with running_files changed
    html =
      render_component(
        FileSelectComponent,
        attrs(file: file, id: "test-loading-running", running_files: [running_file])
      )

    # Loading should not be shown when only running_files changes
    refute html =~ ~s(role="status")
    # Verify basic.livemd is now marked as running
    assert html =~ "play-circle-line"
  end

  defp attrs(attrs) do
    Keyword.merge(
      [
        id: "1",
        file: FileSystem.File.local(p("/")),
        extnames: [".livemd"],
        running_files: []
      ],
      attrs
    )
  end

  defp notebooks_path() do
    Path.expand("../../support/notebooks", __DIR__)
  end
end

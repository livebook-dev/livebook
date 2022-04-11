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

  defp attrs(attrs) do
    Keyword.merge(
      [
        id: 1,
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

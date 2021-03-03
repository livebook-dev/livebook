defmodule LivebookWeb.PathSelectComponentTest do
  use LivebookWeb.ConnCase

  import Phoenix.LiveViewTest

  alias LivebookWeb.PathSelectComponent

  test "when the path has a trailing slash, lists that directory" do
    path = notebooks_path() <> "/"
    assert render_component(PathSelectComponent, attrs(path: path)) =~ "basic.livemd"
    assert render_component(PathSelectComponent, attrs(path: path)) =~ ".."
  end

  test "when the path has no trailing slash, lists the parent directory" do
    path = notebooks_path()
    assert render_component(PathSelectComponent, attrs(path: path)) =~ "notebooks"
  end

  test "lists only files with matching name" do
    path = notebooks_path() |> Path.join("with_two_sectio")
    assert render_component(PathSelectComponent, attrs(path: path)) =~ "with_two_sections.livemd"
    refute render_component(PathSelectComponent, attrs(path: path)) =~ "basic.livemd"
  end

  test "does not show parent directory when in root" do
    path = "/"
    refute render_component(PathSelectComponent, attrs(path: path)) =~ ".."
  end

  test "does not show parent directory when there is a basename typed" do
    path = notebooks_path() |> Path.join("a")
    refute render_component(PathSelectComponent, attrs(path: path)) =~ ".."
  end

  test "relative paths are expanded from the current working directory" do
    File.cd!(notebooks_path())
    path = ""
    assert render_component(PathSelectComponent, attrs(path: path)) =~ "basic.livemd"
  end

  defp attrs(attrs) do
    Keyword.merge(
      [id: 1, path: "/", extnames: [".livemd"], running_paths: [], target: nil],
      attrs
    )
  end

  defp notebooks_path() do
    Path.expand("../../support/notebooks", __DIR__)
  end
end

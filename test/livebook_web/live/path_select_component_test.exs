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

  test "does not show parent directory when in root" do
    path = "/"
    refute render_component(PathSelectComponent, attrs(path: path)) =~ ".."
  end

  test "relative paths are expanded from the current working directory" do
    File.cd!(notebooks_path())
    path = ""
    assert render_component(PathSelectComponent, attrs(path: path)) =~ "basic.livemd"
  end

  defp attrs(attrs) do
    Keyword.merge(
      [
        id: 1,
        path: "/",
        extnames: [".livemd"],
        running_paths: [],
        phx_target: nil,
        phx_submit: nil
      ],
      attrs
    )
  end

  defp notebooks_path() do
    Path.expand("../../support/notebooks", __DIR__)
  end
end

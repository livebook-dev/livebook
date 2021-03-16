defmodule LivebookWeb.FileSystemProviderTest do
  use ExUnit.Case, async: true

  defmodule MyProvider do
    use LivebookWeb.FileSystemProvider,
      from: Path.expand("../../support/static", __DIR__)
  end

  test "includes regular files" do
    assert %{content: content} = MyProvider.get_file(["js", "app.js"], nil)
    assert content == ~s{console.log("Hello");\n}
  end

  test "ignores directories" do
    assert nil == MyProvider.get_file(["js"], nil)
  end

  test "ignores non-existent files" do
    assert nil == MyProvider.get_file(["nonexistent.js"], nil)
  end
end

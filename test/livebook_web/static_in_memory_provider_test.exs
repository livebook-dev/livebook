defmodule LivebookWeb.StaticInMemoryProviderTest do
  use ExUnit.Case, async: true

  # Make sure to run `mix phx.digest test/support/static -o test/support/static`
  # after changing files in `test/support/static`.

  defmodule MyProvider do
    use LivebookWeb.StaticInMemoryProvider,
      from: Path.expand("../support/static", __DIR__),
      # from: :livebook,
      only: ~w(app.js icon.ico)
  end

  test "includes uncompressed file if there is no compressed version" do
    assert true == MyProvider.valid_path?("icon.ico")
    assert %{content: ""} = MyProvider.get_file("icon.ico")
  end

  test "includes compressed version of a file if available" do
    assert true == MyProvider.valid_path?("app.js.gz")
    assert %{content: content} = MyProvider.get_file("app.js.gz")
    assert :zlib.gunzip(content) == ~s{console.log("Hello");\n}
  end

  test "does not include uncompressed file when a compressed version is avaiable" do
    assert false == MyProvider.valid_path?("app.js")
    assert nil == MyProvider.get_file("app.js")
  end

  test "does not include the digested file" do
    assert false == MyProvider.valid_path?("app-3dbaca9bcb436d56b5bd95bf083073a0.js")
    assert nil == MyProvider.get_file("app-3dbaca9bcb436d56b5bd95bf083073a0.js")
  end

  test "excludes files not matched by :only" do
    assert false == MyProvider.valid_path?("excluded.js")
    assert nil == MyProvider.get_file("excluded.js")
  end
end

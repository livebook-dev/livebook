defmodule LivebookWeb.MemoryProviderTest do
  use ExUnit.Case, async: true

  defmodule MyProvider do
    use LivebookWeb.MemoryProvider,
      from: Path.expand("../../support/static", __DIR__),
      gzip: true
  end

  test "includes uncompressed files that are not gzippable" do
    assert %{content: ""} = MyProvider.get_file(["icon.ico"], nil)
  end

  test "includes compressed files which are gzippable" do
    assert %{content: content} = MyProvider.get_file(["js", "app.js"], :gzip)
    assert :zlib.gunzip(content) =~ ~s{console.log("Hello");}
  end

  test "does not include uncompressed files that are gzippable" do
    assert nil == MyProvider.get_file(["js", "app.js"], nil)
  end
end

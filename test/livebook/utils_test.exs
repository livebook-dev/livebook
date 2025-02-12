defmodule Livebook.UtilsTest do
  use ExUnit.Case, async: true
  doctest Livebook.Utils

  describe "expand_desktop_url/1" do
    test "empty url" do
      assert Livebook.Utils.expand_desktop_url("") == "http://localhost:4002/"
    end

    test "/settings" do
      assert Livebook.Utils.expand_desktop_url("/settings") ==
               "http://localhost:4002/settings"
    end

    test "file://" do
      assert Livebook.Utils.expand_desktop_url("file://c/foo.txt") ==
               "http://localhost:4002/open?path=c%2Ffoo.txt"

      assert Livebook.Utils.expand_desktop_url("file://c/../../../foo.txt") ==
               "http://localhost:4002/open?path=c%2F..%2F..%2F..%2Ffoo.txt"

      assert Livebook.Utils.expand_desktop_url("file://c\\foo.txt") ==
               "http://localhost:4002/open?path=c%5Cfoo.txt"

      assert Livebook.Utils.expand_desktop_url("file:///this is a dir/with many spaces/foo.txt") ==
               "http://localhost:4002/open?path=%2Fthis+is+a+dir%2Fwith+many+spaces%2Ffoo.txt"

      assert Livebook.Utils.expand_desktop_url("file://\\\\127.0.0.1\\my folder\\foo.txt") ==
               "http://localhost:4002/open?path=%5C%5C127.0.0.1%5Cmy+folder%5Cfoo.txt"
    end

    test "livebook://" do
      assert Livebook.Utils.expand_desktop_url("livebook://github.com/a/b/blob/main/a.livemd") ==
               "http://localhost:4002/import?url=https%3A%2F%2Fgithub.com%2Fa%2Fb%2Fblob%2Fmain%2Fa.livemd"

      assert Livebook.Utils.expand_desktop_url("livebook://github.com/../../../a.livemd") ==
               "http://localhost:4002/import?url=https%3A%2F%2Fgithub.com%2F..%2F..%2F..%2Fa.livemd"
    end

    test "other urls" do
      assert_raise FunctionClauseError, fn ->
        Livebook.Utils.expand_desktop_url("file:a.txt")
      end

      assert_raise FunctionClauseError, fn ->
        Livebook.Utils.expand_desktop_url("livebook:a.txt")
      end

      assert_raise FunctionClauseError, fn ->
        Livebook.Utils.expand_desktop_url("a.txt")
      end
    end
  end
end

defmodule LiveBookWeb.HelpersTest do
  use ExUnit.Case, async: true

  alias LiveBookWeb.Helpers

  doctest Helpers

  describe "inspect_as_html/2" do
    test "uses span tags for term highlighting" do
      assert {:safe,
              ~s{<span class="list">[</span><span class="number">1</span><span class="list">,</span> <span class="number">2</span><span class="list">]</span>}} ==
               Helpers.inspect_as_html([1, 2])
    end

    test "escapes HTML in the inspect result" do
      assert {:safe, ~s{<span class="string">&quot;1 &lt; 2&quot;</span>}} ==
               Helpers.inspect_as_html("1 < 2")
    end
  end
end

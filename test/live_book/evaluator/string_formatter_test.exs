defmodule LiveBook.Evaluator.StringFormatterTest do
  use ExUnit.Case, async: true

  alias LiveBook.Evaluator.StringFormatter

  doctest StringFormatter

  describe "inspect_as_html/2" do
    test "uses span tags for term highlighting" do
      assert ~s{<span class="list">[</span><span class="number">1</span><span class="list">,</span> <span class="number">2</span><span class="list">]</span>} ==
               StringFormatter.inspect_as_html([1, 2])
    end

    test "escapes HTML in the inspect result" do
      assert ~s{<span class="string">&quot;1 &lt; 2&quot;</span>} ==
               StringFormatter.inspect_as_html("1 < 2")
    end
  end
end

defmodule LivebookWeb.HelpersTest do
  use ExUnit.Case, async: true

  alias LivebookWeb.Helpers

  describe "ansi_to_html_lines/1" do
    test "puts every line in its own tag" do
      assert [
               {:safe, ~s{<span style="color: var(--ansi-color-blue);">smiley</span>}},
               {:safe, ~s{<span style="color: var(--ansi-color-blue);">cat</span>}}
             ] ==
               Helpers.ansi_to_html_lines("\e[34msmiley\ncat\e[0m")
    end
  end
end

defmodule LiveBookWeb.HelpersTest do
  use ExUnit.Case, async: true

  alias LiveBookWeb.Helpers

  describe "ansi_string_to_html/1" do
    test "converts ANSI escape codes to span tags" do
      assert ~s{<span class="ansi blue">:cat</span>} ==
               Helpers.ansi_string_to_html("\e[34m:cat\e[0m") |> Phoenix.HTML.safe_to_string()
    end

    test "escapes HTML in the inspect result" do
      assert ~s{&lt;div&gt;} ==
               Helpers.ansi_string_to_html("<div>") |> Phoenix.HTML.safe_to_string()
    end
  end
end

defmodule Livebook.Utils.ANSITest do
  use ExUnit.Case, async: true

  alias Livebook.Utils.ANSI

  describe "parse_ansi_string/1" do
    test "converts ANSI escape codes to predefined modifiers" do
      assert ANSI.parse_ansi_string("\e[34mcat\e[0m") == [{[foreground_color: :blue], "cat"}]
      assert ANSI.parse_ansi_string("\e[44mcat\e[0m") == [{[background_color: :blue], "cat"}]
      assert ANSI.parse_ansi_string("\e[1mcat\e[0m") == [{[font_weight: :bold], "cat"}]
      assert ANSI.parse_ansi_string("\e[4mcat\e[0m") == [{[text_decoration: :underline], "cat"}]
    end

    test "supports short reset sequence" do
      assert ANSI.parse_ansi_string("\e[34mcat\e[m") == [{[foreground_color: :blue], "cat"}]
    end

    test "supports multiple escape codes at the same time" do
      assert ANSI.parse_ansi_string("\e[34m\e[41mcat\e[0m") ==
               [{[background_color: :red, foreground_color: :blue], "cat"}]
    end

    test "overriding a particular style property keeps the others" do
      assert [{[background_color: :red], "cat"}] ==
               ANSI.parse_ansi_string("\e[34m\e[41m\e[39mcat\e[0m")
    end

    test "adjacent content with the same properties is wrapped in a single pair" do
      assert ANSI.parse_ansi_string("\e[34mcool\e[0m\e[34mcats\e[0m") ==
               [{[foreground_color: :blue], "coolcats"}]
    end

    test "modifiers have effect until reset" do
      assert ANSI.parse_ansi_string("\e[34mcool\e[4mcats\e[0m") ==
               [
                 {[foreground_color: :blue], "cool"},
                 {[foreground_color: :blue, text_decoration: :underline], "cats"}
               ]
    end

    test "supports 8-bit rgb colors" do
      assert ANSI.parse_ansi_string("\e[38;5;67mcat\e[0m") ==
               [{[foreground_color: {:rgb6, 1, 2, 3}], "cat"}]
    end

    test "supports 8-bit grayscale range" do
      assert ANSI.parse_ansi_string("\e[38;5;240mcat\e[0m") ==
               [{[foreground_color: {:grayscale24, 8}], "cat"}]
    end

    test "supports 8-bit well known colors" do
      assert ANSI.parse_ansi_string("\e[38;5;1mcat\e[0m") ==
               [{[foreground_color: :red], "cat"}]
    end

    test "ignores valid but irrelevant escape codes" do
      assert ANSI.parse_ansi_string("\e[H\e[1Acat") == [{[], "cat"}]
    end

    test "returns the whole string if on ANSI code detected" do
      assert ANSI.parse_ansi_string("\e[300mcat") == [{[], "\e[300mcat"}]
    end

    test "ignores RFC 1468 switch to ASCII" do
      assert ANSI.parse_ansi_string("\e(Bcat") == [{[], "cat"}]
    end
  end
end

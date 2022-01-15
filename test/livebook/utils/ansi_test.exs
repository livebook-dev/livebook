defmodule Livebook.Utils.ANSITest do
  use ExUnit.Case, async: true

  alias Livebook.Utils.ANSI

  describe "parse_ansi_string/1" do
    test "converts ANSI escape codes to predefined modifiers" do
      assert {[{[foreground_color: :blue], "cat"}], _modifiers} =
               ANSI.parse_ansi_string("\e[34mcat\e[0m")

      assert {[{[background_color: :blue], "cat"}], _modifiers} =
               ANSI.parse_ansi_string("\e[44mcat\e[0m")

      assert {[{[font_weight: :bold], "cat"}], _modifiers} =
               ANSI.parse_ansi_string("\e[1mcat\e[0m")

      assert {[{[text_decoration: :underline], "cat"}], _modifiers} =
               ANSI.parse_ansi_string("\e[4mcat\e[0m")
    end

    test "supports short reset sequence" do
      assert {[{[foreground_color: :blue], "cat"}], _modifiers} =
               ANSI.parse_ansi_string("\e[34mcat\e[m")
    end

    test "supports multiple escape codes at the same time" do
      assert {[{[background_color: :red, foreground_color: :blue], "cat"}], _modifiers} =
               ANSI.parse_ansi_string("\e[34m\e[41mcat\e[0m")
    end

    test "overriding a particular style property keeps the others" do
      assert {[{[background_color: :red], "cat"}], _modifiers} =
               ANSI.parse_ansi_string("\e[34m\e[41m\e[39mcat\e[0m")
    end

    test "adjacent content with the same properties is wrapped in a single pair" do
      assert {[{[foreground_color: :blue], "coolcats"}], _modifiers} =
               ANSI.parse_ansi_string("\e[34mcool\e[0m\e[34mcats\e[0m")
    end

    test "modifiers have effect until reset" do
      assert {[
                {[foreground_color: :blue], "cool"},
                {[text_decoration: :underline, foreground_color: :blue], "cats"}
              ], _modifiers} = ANSI.parse_ansi_string("\e[34mcool\e[4mcats\e[0m")
    end

    test "supports 8-bit rgb colors" do
      assert {[{[foreground_color: {:rgb6, 1, 2, 3}], "cat"}], _modifiers} =
               ANSI.parse_ansi_string("\e[38;5;67mcat\e[0m")
    end

    test "supports 8-bit grayscale range" do
      assert {[{[foreground_color: {:grayscale24, 8}], "cat"}], _modifiers} =
               ANSI.parse_ansi_string("\e[38;5;240mcat\e[0m")
    end

    test "supports 8-bit well known colors" do
      assert {[{[foreground_color: :red], "cat"}], _modifiers} =
               ANSI.parse_ansi_string("\e[38;5;1mcat\e[0m")
    end

    test "ignores valid but irrelevant escape codes" do
      assert {[{[], "cat"}], _modifiers} = ANSI.parse_ansi_string("\e[H\e[1Acat")
    end

    test "returns the whole string if no ANSI code is detected" do
      assert {[{[], "\e[300mcat"}], _modifiers} = ANSI.parse_ansi_string("\e[300mcat")
      assert {[{[], "\ehmmcat"}], _modifiers} = ANSI.parse_ansi_string("\ehmmcat")
    end

    test "ignores RFC 1468 switch to ASCII" do
      assert {[{[], "cat"}], _modifiers} = ANSI.parse_ansi_string("\e(Bcat")
    end

    test "supports multiple codes separated by semicolon" do
      assert {[{[foreground_color: :blue], "cat"}], _modifiers} =
               ANSI.parse_ansi_string("\e[0;34mcat")

      assert {[{[background_color: :red, foreground_color: :blue], "cat"}], _modifiers} =
               ANSI.parse_ansi_string("\e[34;41mcat\e[0m")

      # 8-bit rgb color followed by background color
      assert {[{[background_color: :red, foreground_color: {:rgb6, 1, 2, 3}], "cat"}], _modifiers} =
               ANSI.parse_ansi_string("\e[38;5;67;41mcat\e[0m")
    end
  end
end

defmodule LiveBookWeb.ANSITest do
  use ExUnit.Case, async: true

  alias LiveBookWeb.ANSI

  describe "ansi_string_to_html/1" do
    test "converts ANSI escape codes to span tags" do
      assert ~s{<span style="color: var(--ansi-color-blue);">cat</span>} ==
               ANSI.ansi_string_to_html("\e[34mcat\e[0m") |> Phoenix.HTML.safe_to_string()

      assert ~s{<span style="background-color: var(--ansi-color-blue);">cat</span>} ==
               ANSI.ansi_string_to_html("\e[44mcat\e[0m") |> Phoenix.HTML.safe_to_string()

      assert ~s{<span style="font-weight: 600;">cat</span>} ==
               ANSI.ansi_string_to_html("\e[1mcat\e[0m") |> Phoenix.HTML.safe_to_string()

      assert ~s{<span style="text-decoration: underline;">cat</span>} ==
               ANSI.ansi_string_to_html("\e[4mcat\e[0m") |> Phoenix.HTML.safe_to_string()
    end

    test "supports multiple escape codes at the same time" do
      assert ~s{<span style="background-color: var(--ansi-color-red);color: var(--ansi-color-blue);">cat</span>} ==
               ANSI.ansi_string_to_html("\e[34m\e[41mcat\e[0m") |> Phoenix.HTML.safe_to_string()
    end

    test "overriding a particular style property keeps the others" do
      assert ~s{<span style="background-color: var(--ansi-color-red);">cat</span>} ==
               ANSI.ansi_string_to_html("\e[34m\e[41m\e[39mcat\e[0m")
               |> Phoenix.HTML.safe_to_string()
    end

    test "adjacent content with the same properties is wrapped in a single element" do
      assert ~s{<span style="color: var(--ansi-color-blue);">coolcats</span>} ==
               ANSI.ansi_string_to_html("\e[34mcool\e[0m\e[34mcats\e[0m")
               |> Phoenix.HTML.safe_to_string()
    end

    test "modifiers have effect until reset" do
      assert ~s{<span style="color: var(--ansi-color-blue);">cool</span><span style="color: var(--ansi-color-blue);text-decoration: underline;">cats</span>} ==
               ANSI.ansi_string_to_html("\e[34mcool\e[4mcats\e[0m")
               |> Phoenix.HTML.safe_to_string()
    end

    test "supports 8-bit rgb colors" do
      assert ~s{<span style="color: rgb(51, 102, 153);">cat</span>} ==
               ANSI.ansi_string_to_html("\e[38;5;67mcat\e[0m") |> Phoenix.HTML.safe_to_string()
    end

    test "supports 8-bit grayscale range" do
      assert ~s{<span style="color: rgb(88, 88, 88);">cat</span>} ==
               ANSI.ansi_string_to_html("\e[38;5;240mcat\e[0m") |> Phoenix.HTML.safe_to_string()
    end

    test "supports 8-bit well known colors" do
      assert ~s{<span style="color: var(--ansi-color-red);">cat</span>} ==
               ANSI.ansi_string_to_html("\e[38;5;1mcat\e[0m") |> Phoenix.HTML.safe_to_string()
    end

    test "ignores valid but irrelevant escape codes" do
      assert ~s{cat} == ANSI.ansi_string_to_html("\e[H\e[1Acat") |> Phoenix.HTML.safe_to_string()
    end

    test "returns the whole string if on ANSI code detected" do
      assert ~s{\e[300mcat} ==
               ANSI.ansi_string_to_html("\e[300mcat") |> Phoenix.HTML.safe_to_string()
    end

    test "escapes HTML in the resulting string" do
      assert ~s{&lt;div&gt;} == ANSI.ansi_string_to_html("<div>") |> Phoenix.HTML.safe_to_string()
    end
  end
end

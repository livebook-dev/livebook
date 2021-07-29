defmodule LivebookWeb.Helpers.ANSITest do
  use ExUnit.Case, async: true

  alias LivebookWeb.Helpers.ANSI

  describe "ansi_string_to_html/2" do
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

    test "renders 8-bit rgb colors as regular rgb" do
      assert ~s{<span style="color: rgb(51, 102, 153);">cat</span>} ==
               ANSI.ansi_string_to_html("\e[38;5;67mcat\e[0m") |> Phoenix.HTML.safe_to_string()
    end

    test "renders 8-bit grayscale as regular rgb" do
      assert ~s{<span style="color: rgb(88, 88, 88);">cat</span>} ==
               ANSI.ansi_string_to_html("\e[38;5;240mcat\e[0m") |> Phoenix.HTML.safe_to_string()
    end

    test "escapes HTML in the resulting string" do
      assert ~s{&lt;div&gt;} == ANSI.ansi_string_to_html("<div>") |> Phoenix.HTML.safe_to_string()
    end
  end

  describe "ansi_string_to_html_lines/1" do
    test "renders every line as complete HTML" do
      assert ["cool", "cat"] ==
               ANSI.ansi_string_to_html_lines("cool\ncat")
               |> Enum.map(&Phoenix.HTML.safe_to_string/1)

      assert [
               ~s{<span style="color: var(--ansi-color-blue);">cool</span>},
               ~s{<span style="color: var(--ansi-color-blue);">cat</span>}
             ] ==
               ANSI.ansi_string_to_html_lines("\e[34mcool\ncat\e[0m")
               |> Enum.map(&Phoenix.HTML.safe_to_string/1)

      assert [
               ~s{<span style="color: var(--ansi-color-blue);">cool</span><span style="color: var(--ansi-color-green);">cats</span>},
               ~s{chillin}
             ] ==
               ANSI.ansi_string_to_html_lines("\e[34mcool\e[32mcats\n\e[0mchillin")
               |> Enum.map(&Phoenix.HTML.safe_to_string/1)
    end
  end
end

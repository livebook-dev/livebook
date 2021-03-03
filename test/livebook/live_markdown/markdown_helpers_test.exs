defmodule Livebook.LiveMarkdown.MarkdownHelpersTest do
  use ExUnit.Case, async: true

  alias Livebook.LiveMarkdown.MarkdownHelpers

  describe "markdown_from_ast/1" do
    test "emphasis" do
      markdown = "The Game, *Mrs Hudson*, is on!"
      assert markdown == reformat(markdown)
    end

    test "bold" do
      markdown = "The Game, **Mrs Hudson**, is on!"
      assert markdown == reformat(markdown)
    end

    test "strikethrough" do
      markdown = "The Game, ~~Mrs Hudson~~, is on!"
      assert markdown == reformat(markdown)
    end

    test "inline code" do
      markdown = "The Game, `Mrs Hudson`, is on!"
      assert markdown == reformat(markdown)
    end

    test "combined" do
      markdown = "The Game, ~~***`Mrs Hudson`***~~, is on!"
      assert markdown == reformat(markdown)
    end

    test "link" do
      markdown = "The Game, [Mrs Hudson](https://youtu.be/M-KqaO1oH2E), is on!"
      assert markdown == reformat(markdown)
    end

    test "basic image" do
      markdown = "The Game, ![Mrs Hudson](https://example.com), is on!"
      assert markdown == reformat(markdown)
    end

    test "image with title" do
      markdown = "The Game, ![Mrs Hudson](https://example.com \"Title\"), is on!"
      assert markdown == reformat(markdown)
    end

    test "img tag with additional attributes is kept as a tag" do
      markdown = ~s{<img src="https://example.com" alt="Mrs Hudson" width="300" />}
      assert markdown == reformat(markdown)
    end

    test "oneline comment" do
      markdown = "<!-- The Game, Mrs Hudson, is on! -->"
      assert markdown == reformat(markdown)
    end

    test "multiline comment" do
      markdown = """
      <!--
      The Game, Mrs Hudson,
      is on!
      -->\
      """

      assert markdown == reformat(markdown)
    end

    test "ruler" do
      markdown = "---"
      assert markdown == reformat(markdown)
    end

    test "paragraph" do
      markdown = """
      First paragrpah.

      Second paragraph.\
      """

      assert markdown == reformat(markdown)
    end

    test "heading" do
      markdown = """
      # Heading 1

      ## Heading 2

      ### Heading 3

      #### Heading 4

      ##### Heading 5

      ###### Heading 6\
      """

      assert markdown == reformat(markdown)
    end

    test "code block" do
      markdown = """
      ```elixir
      Enum.to_list(1..10)
      ```\
      """

      assert markdown == reformat(markdown)
    end

    test "blockquote" do
      markdown = """
      > The Game, Mrs Hudson,
      > is on!\
      """

      assert markdown == reformat(markdown)
    end

    test "table with header" do
      markdown = """
      | State | Abbrev | Capital |
      | ----: | :----: | ------- |
      | Texas | TX     | Austin  |
      | Maine | ME     | Augusta |\
      """

      assert markdown == reformat(markdown)
    end

    test "table without header" do
      markdown = """
      | Texas | TX | Austin  |
      | Maine | ME | Augusta |\
      """

      assert markdown == reformat(markdown)
    end

    test "basic unordered list" do
      markdown = """
      * Olafur Arnalds
      * Hans Zimmer
      * Philip Glass\
      """

      assert markdown == reformat(markdown)
    end

    test "spaced unordered list" do
      markdown = """
      * Olafur Arnalds

      * Hans Zimmer

      * Philip Glass\
      """

      assert markdown == reformat(markdown)
    end

    test "unordered list with block items" do
      markdown = """
      * Quote

        > We can't control what happens to us, only how it affects us and the choices we make.

      * Code

        ```elixir
        Enum.to_list(1..10)
        ```\
      """

      assert markdown == reformat(markdown)
    end

    test "nested unordered list" do
      markdown = """
      * Olafur Arnalds
        * Particles
        * Doria
      * Hans Zimmer
        * Time
      * Philip Glass
        * Opening
        * The Poet Acts\
      """

      assert markdown == reformat(markdown)
    end

    test "basic ordered list" do
      markdown = """
      1. Olafur Arnalds
      2. Hans Zimmer
      3. Philip Glass\
      """

      assert markdown == reformat(markdown)
    end

    test "spaced ordered list" do
      markdown = """
      1. Olafur Arnalds

      2. Hans Zimmer

      3. Philip Glass\
      """

      assert markdown == reformat(markdown)
    end

    test "with block items ordered list" do
      markdown = """
      1. Quote

         > We can't control what happens to us, only how it affects us and the choices we make.

      2. Code

         ```elixir
         Enum.to_list(1..10)
         ```\
      """

      assert markdown == reformat(markdown)
    end

    test "ordered list: nested" do
      markdown = """
      1. Olafur Arnalds
         1. Particles
         2. Doria
      2. Hans Zimmer
         1. Time
      3. Philip Glass
         1. Opening
         2. The Poet Acts\
      """

      assert markdown == reformat(markdown)
    end

    test "raw html" do
      markdown = """
      <div class="box" aria-label="box">
        Some content
      </div>\
      """

      assert markdown == reformat(markdown)
    end

    test "separates blocks with a single line" do
      markdown = """
      The first paragraph,
      with multiple lines.

      > We can't control what happens to us,
      > only how it affects us and the choices we make.

      ```elixir
      Enum.to_list(1..10)
      ```

      Another paragraph.\
      """

      assert markdown == reformat(markdown)
    end

    # By reformatting we can assert correct rendering
    # by comparing against the original content.
    defp reformat(markdown) do
      {:ok, ast, []} = EarmarkParser.as_ast(markdown)
      MarkdownHelpers.markdown_from_ast(ast)
    end
  end
end

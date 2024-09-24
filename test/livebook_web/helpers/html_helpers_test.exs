defmodule LivebookWeb.HTMLHelpersTest do
  use ExUnit.Case, async: true

  alias LivebookWeb.HTMLHelpers

  doctest HTMLHelpers

  describe "names_to_html_ids/1" do
    test "title case" do
      assert(HTMLHelpers.names_to_html_ids(["Title of a Section"]) == ["title-of-a-section"])
    end

    # Contains a couple of unicode spaces to ensure that we handle those
    test "space characters" do
      assert HTMLHelpers.names_to_html_ids(["  slug \n   with  spaces \t  "]) == [
               "slug-with-spaces"
             ]
    end

    test "emoji at end" do
      assert HTMLHelpers.names_to_html_ids(["Test 🦦 "]) == ["test"]
    end

    test "emoji in middle" do
      assert HTMLHelpers.names_to_html_ids(["One 🥮 Two"]) == ["one-two"]
    end

    test "returns empty list for an empty list" do
      assert HTMLHelpers.names_to_html_ids([]) == []
    end

    test "returns id-ified strings for different kinds of names" do
      names = [
        "Title of a Section",
        "  something with \n  many  space  characters \t  "
      ]

      assert HTMLHelpers.names_to_html_ids(names) == [
               "title-of-a-section",
               "something-with-many-space-characters"
             ]
    end

    test "enumerates ids when they would be the same" do
      names = [
        "Title of a Section",
        "Some other title",
        " Title of a  Section",
        "random",
        " Title of a  section",
        "Title  of a Section "
      ]

      assert HTMLHelpers.names_to_html_ids(names) == [
               "title-of-a-section",
               "some-other-title",
               "title-of-a-section-2",
               "random",
               "title-of-a-section-3",
               "title-of-a-section-4"
             ]
    end
  end
end

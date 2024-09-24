defmodule Livebook.Text.JSTest do
  use ExUnit.Case, async: true

  alias Livebook.Text.JS

  describe "length/1" do
    test "counts ASCII characters" do
      string = "fox in a box"
      assert JS.length(string) == 12
    end

    test "counts characters spanning multiple UTF-16 code units" do
      # 🦊 and 📦 consist of 2 UTF-16 code units
      string = "🦊 in a 📦"
      assert JS.length(string) == 10
    end

    test "counts combining characters separately" do
      # This is a single grapheme cluster, but two separate Unicode
      # code points, one UTF-16 code unit each
      string = "\u0065\u0301"
      assert JS.length(string) == 2
    end
  end

  describe "slice/2" do
    test "slices ASCII characters" do
      string = "fox in a box"
      assert JS.slice(string, 4) == "in a box"
    end

    test "slices characters spanning multiple UTF-16 code units" do
      string = "🦊 in a 📦"
      assert JS.slice(string, 3) == "in a 📦"
    end

    test "slices combining characters" do
      string = "\u0065\u0301"
      assert JS.slice(string, 1) == "\u0301"
    end
  end

  describe "slice/3" do
    test "slices ASCII characters" do
      string = "fox in a box"
      assert JS.slice(string, 4, 2) == "in"
    end

    test "slices characters spanning multiple UTF-16 code units" do
      string = "🦊 in a 📦"
      assert JS.slice(string, 0, 5) == "🦊 in"
    end

    test "slices combining characters" do
      string = "\u0065\u0301"
      assert JS.slice(string, 0, 1) == "\u0065"
    end
  end

  describe "split_at/2" do
    test "splits ASCII characters" do
      string = "fox in a box"
      assert JS.split_at(string, 6) == {"fox in", " a box"}
    end

    test "splits characters spanning multiple UTF-16 code units" do
      string = "🦊 in a 📦"
      assert JS.split_at(string, 5) == {"🦊 in", " a 📦"}
    end

    test "splits combining characters" do
      string = "\u0065\u0301"
      assert JS.split_at(string, 1) == {"\u0065", "\u0301"}
    end
  end

  describe "js_column_to_elixir/2" do
    test "keeps the column as is for ASCII characters" do
      column = 4
      line = "String.replace"
      assert JS.js_column_to_elixir(column, line) == 4
    end

    test "shifts the column given characters spanning multiple UTF-16 code units" do
      column = 7
      line = "🦊🦊 String.replace"
      assert JS.js_column_to_elixir(column, line) == 5
    end

    test "returns proper column if the first of a UTF-16 code unit pair is given" do
      column = 3
      line = "🦊🦊 String.replace"
      assert JS.js_column_to_elixir(column, line) == 2
    end

    test "returns proper column if the second of a UTF-16 code unit pair is given" do
      column = 4
      line = "🦊🦊 String.replace"
      assert JS.js_column_to_elixir(column, line) == 2
    end

    test "returns proper column if a combining Unicode characters is given" do
      column = 2
      line = "\u0065\u0301 String.replace"
      assert JS.js_column_to_elixir(column, line) == 1
    end
  end

  describe "elixir_column_to_js/2" do
    test "keeps the column as is for ASCII characters" do
      column = 4
      line = "String.replace"
      assert JS.elixir_column_to_js(column, line) == 4
    end

    test "shifts the column given characters spanning multiple UTF-16 code units" do
      column = 5
      line = "🦊🦊 String.replace"
      assert JS.elixir_column_to_js(column, line) == 7
    end

    test "returns column corresponding to the first UTF-16 code unit" do
      column = 2
      line = "🦊🦊 String.replace"
      assert JS.elixir_column_to_js(column, line) == 3
    end

    test "returns column corresponding to the first character in a grapheme cluster" do
      column = 1
      line = "\u0065\u0301 String.replace"
      assert JS.elixir_column_to_js(column, line) == 1
    end
  end
end

defmodule LivebookTest do
  use ExUnit.Case, async: true

  test "live_markdown_to_elixir/1" do
    markdown = """
    # Lists

    ## Introduction

    Let's generate a list of numbers:

    ```elixir
    Enum.to_list(1..10)
    ```
    """

    assert Livebook.live_markdown_to_elixir(markdown) == """
           # Run as: iex --dot-iex path/to/notebook.exs

           # Title: Lists

           # ── Introduction ──

           # Let's generate a list of numbers:

           Enum.to_list(1..10)
           """
  end
end

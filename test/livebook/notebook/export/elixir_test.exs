defmodule Livebook.Notebook.Export.ElixirTest do
  use ExUnit.Case, async: true

  alias Livebook.Notebook.Export
  alias Livebook.Notebook

  test "acceptance" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              cells: [
                %{
                  Notebook.Cell.new(:markdown)
                  | source: """
                    Make sure to install:

                    * Erlang
                    * Elixir
                    * PostgreSQL\
                    """
                },
                %{
                  Notebook.Cell.new(:elixir)
                  | disable_formatting: true,
                    source: """
                    Enum.to_list(1..10)\
                    """
                },
                %{
                  Notebook.Cell.new(:markdown)
                  | source: """
                    This is it for this section.\
                    """
                }
              ]
          },
          %{
            Notebook.Section.new()
            | id: "s2",
              name: "Section 2",
              cells: [
                %{
                  Notebook.Cell.new(:elixir)
                  | source: """
                    IO.gets("length: ")\
                    """
                }
              ]
          },
          %{
            Notebook.Section.new()
            | name: "Section 3",
              parent_id: "s2",
              cells: [
                %{
                  Notebook.Cell.new(:elixir)
                  | source: """
                    Process.info()\
                    """
                }
              ]
          }
        ]
    }

    expected_document = """
    # Title: My Notebook

    # ── Section 1 ──

    # Make sure to install:

    # * Erlang
    # * Elixir
    # * PostgreSQL

    Enum.to_list(1..10)

    # This is it for this section.

    # ── Section 2 ──

    IO.gets("length: ")

    # ── Section 3 ── (⎇ from Section 2)

    # Process.info()
    """

    document = Export.Elixir.notebook_to_elixir(notebook)

    assert expected_document == document
  end
end

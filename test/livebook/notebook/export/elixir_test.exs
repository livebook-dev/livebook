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
                  Notebook.Cell.new(:code)
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
                  Notebook.Cell.new(:code)
                  | source: """
                    IO.gets("length: ")\
                    """
                },
                %{
                  Notebook.Cell.new(:smart)
                  | source: """
                    IO.puts("My text")\
                    """,
                    attrs: %{"text" => "My text"},
                    kind: "text"
                }
              ]
          },
          %{
            Notebook.Section.new()
            | name: "Section 3",
              parent_id: "s2",
              cells: [
                %{
                  Notebook.Cell.new(:code)
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

    IO.puts("My text")

    # ── Section 3 ── (⎇ from Section 2)

    # Process.info()
    """

    document = Export.Elixir.notebook_to_elixir(notebook)

    assert expected_document == document
  end

  describe "setup cell" do
    test "includes the leading setup cell when it has content" do
      notebook =
        %{
          Notebook.new()
          | name: "My Notebook",
            sections: [%{Notebook.Section.new() | name: "Section 1"}]
        }
        |> Notebook.put_setup_cell(%{Notebook.Cell.new(:code) | source: "Mix.install([...])"})

      expected_document = """
      # Title: My Notebook

      Mix.install([...])

      # ── Section 1 ──
      """

      document = Export.Elixir.notebook_to_elixir(notebook)

      assert expected_document == document
    end
  end
end

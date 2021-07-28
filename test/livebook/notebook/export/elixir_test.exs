defmodule Livebook.Notebook.Export.ElixirTest do
  use ExUnit.Case, async: true

  alias Livebook.Notebook.Export
  alias Livebook.Notebook

  test "acceptance" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        metadata: %{"author" => "Sherlock Holmes"},
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              metadata: %{"created_at" => "2021-02-15"},
              cells: [
                %{
                  Notebook.Cell.new(:markdown)
                  | metadata: %{"updated_at" => "2021-02-15"},
                    source: """
                    Make sure to install:

                    * Erlang
                    * Elixir
                    * PostgreSQL\
                    """
                },
                %{
                  Notebook.Cell.new(:elixir)
                  | metadata: %{"readonly" => true},
                    source: """
                    Enum.to_list(1..10)\
                    """
                },
                %{
                  Notebook.Cell.new(:markdown)
                  | metadata: %{},
                    source: """
                    This is it for this section.\
                    """
                }
              ]
          },
          %{
            Notebook.Section.new()
            | id: "s2",
              name: "Section 2",
              metadata: %{},
              cells: [
                %{
                  Notebook.Cell.new(:input)
                  | type: :text,
                    name: "length",
                    value: "100",
                    reactive: true
                },
                %{
                  Notebook.Cell.new(:elixir)
                  | metadata: %{},
                    source: """
                    IO.gets("length: ")\
                    """
                },
                %{
                  Notebook.Cell.new(:input)
                  | type: :range,
                    name: "length",
                    value: "100",
                    props: %{min: 50, max: 150, step: 2}
                }
              ]
          },
          %{
            Notebook.Section.new()
            | name: "Section 3",
              metadata: %{},
              parent_id: "s2",
              cells: [
                %{
                  Notebook.Cell.new(:elixir)
                  | metadata: %{},
                    source: """
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

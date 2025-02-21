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
                  | source: """
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
    # Run as: iex --dot-iex path/to/notebook.exs

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
        |> Notebook.put_setup_cells([%{Notebook.Cell.new(:code) | source: "Mix.install([...])"}])

      expected_document = """
      # Run as: iex --dot-iex path/to/notebook.exs

      # Title: My Notebook

      Mix.install([...])

      # ── Section 1 ──
      """

      document = Export.Elixir.notebook_to_elixir(notebook)

      assert expected_document == document
    end
  end

  test "comments out non-elixir code cells" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              cells: [
                %{
                  Notebook.Cell.new(:code)
                  | source: """
                    Enum.to_list(1..10)\
                    """
                },
                %{
                  Notebook.Cell.new(:code)
                  | language: :erlang,
                    source: """
                    lists:seq(1, 10).\
                    """
                }
              ]
          }
        ]
    }

    expected_document = """
    # Run as: iex --dot-iex path/to/notebook.exs

    # Title: My Notebook

    # ── Section 1 ──

    Enum.to_list(1..10)

    # lists:seq(1, 10).
    """

    document = Export.Elixir.notebook_to_elixir(notebook)

    assert expected_document == document
  end

  test "python" do
    notebook =
      %{
        Notebook.new()
        | name: "My Notebook",
          sections: [
            %{
              Notebook.Section.new()
              | name: "Section 1",
                cells: [
                  %{
                    Notebook.Cell.new(:code)
                    | language: :python,
                      source: """
                      range(0, 10)\
                      """
                  }
                ]
            }
          ]
      }
      |> Notebook.put_setup_cells([
        %{
          Notebook.Cell.new(:code)
          | source: """
            Mix.install([
              {:pythonx, "~> 0.4.0"}
            ])\
            """
        },
        %{
          Notebook.Cell.new(:code)
          | language: :"pyproject.toml",
            source: """
            [project]
            name = "project"
            version = "0.0.0"
            requires-python = "==3.13.*"
            dependencies = []\
            """
        }
      ])

    expected_document = ~S'''
    # Run as: iex --dot-iex path/to/notebook.exs

    # Title: My Notebook

    Mix.install([
      {:pythonx, "~> 0.4.0"}
    ])

    Pythonx.uv_init("""
    [project]
    name = "project"
    version = "0.0.0"
    requires-python = "==3.13.*"
    dependencies = []
    """)

    import Pythonx

    # ── Section 1 ──

    ~PY"""
    range(0, 10)
    """
    '''

    document = Export.Elixir.notebook_to_elixir(notebook)

    assert expected_document == document
  end
end

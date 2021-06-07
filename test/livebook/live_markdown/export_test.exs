defmodule Livebook.LiveMarkdown.ExportTest do
  use ExUnit.Case, async: true

  alias Livebook.LiveMarkdown.Export
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
            | name: "Section 2",
              metadata: %{},
              cells: [
                %{
                  Notebook.Cell.new(:input)
                  | type: :text,
                    name: "length",
                    value: "100"
                },
                %{
                  Notebook.Cell.new(:elixir)
                  | metadata: %{},
                    source: """
                    IO.gets("length: ")
                    """
                }
              ]
          }
        ]
    }

    expected_document = """
    <!-- livebook:{"author":"Sherlock Holmes"} -->

    # My Notebook

    <!-- livebook:{"created_at":"2021-02-15"} -->

    ## Section 1

    <!-- livebook:{"updated_at":"2021-02-15"} -->

    Make sure to install:

    * Erlang
    * Elixir
    * PostgreSQL

    <!-- livebook:{"readonly":true} -->

    ```elixir
    Enum.to_list(1..10)
    ```

    This is it for this section.

    ## Section 2

    <!-- livebook:{"livebook_object":"cell_input","name":"length","type":"text","value":"100"} -->

    ```elixir
    IO.gets("length: ")
    ```
    """

    document = Export.notebook_to_markdown(notebook)

    assert expected_document == document
  end

  test "reformats markdown cells" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        metadata: %{},
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              metadata: %{},
              cells: [
                %{
                  Notebook.Cell.new(:markdown)
                  | metadata: %{},
                    source: """
                    |State|Abbrev|Capital|
                    | --: | :-: | --- |
                    | Texas | TX | Austin |
                    | Maine | ME | Augusta |
                    """
                }
              ]
          }
        ]
    }

    expected_document = """
    # My Notebook

    ## Section 1

    | State | Abbrev | Capital |
    | ----: | :----: | ------- |
    | Texas | TX     | Austin  |
    | Maine | ME     | Augusta |
    """

    document = Export.notebook_to_markdown(notebook)

    assert expected_document == document
  end

  test "drops heading 1 and 2 in markdown cells" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        metadata: %{},
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              metadata: %{},
              cells: [
                %{
                  Notebook.Cell.new(:markdown)
                  | metadata: %{},
                    source: """
                    # Heading 1

                    ## Heading 2

                    ### Heading 3
                    """
                }
              ]
          }
        ]
    }

    expected_document = """
    # My Notebook

    ## Section 1

    ### Heading 3
    """

    document = Export.notebook_to_markdown(notebook)

    assert expected_document == document
  end

  test "keeps non-elixir code snippets" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        metadata: %{},
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              metadata: %{},
              cells: [
                %{
                  Notebook.Cell.new(:markdown)
                  | metadata: %{},
                    source: """
                    ```shell
                    mix deps.get
                    ```

                    ```erlang
                    spawn_link(fun() -> io:format("Hiya") end).
                    ```
                    """
                }
              ]
          }
        ]
    }

    expected_document = """
    # My Notebook

    ## Section 1

    ```shell
    mix deps.get
    ```

    ```erlang
    spawn_link(fun() -> io:format("Hiya") end).
    ```
    """

    document = Export.notebook_to_markdown(notebook)

    assert expected_document == document
  end

  test "marks elixir snippets in markdown cells as such" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        metadata: %{},
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              metadata: %{},
              cells: [
                %{
                  Notebook.Cell.new(:markdown)
                  | metadata: %{},
                    source: """
                    ```elixir
                    [1, 2, 3]
                    ```\
                    """
                }
              ]
          },
          %{
            Notebook.Section.new()
            | name: "Section 2",
              metadata: %{},
              cells: [
                %{
                  Notebook.Cell.new(:markdown)
                  | metadata: %{},
                    source: """
                    Some markdown.

                    ```elixir
                    [1, 2, 3]
                    ```\
                    """
                }
              ]
          }
        ]
    }

    expected_document = """
    # My Notebook

    ## Section 1

    <!-- livebook:{"force_markdown":true} -->

    ```elixir
    [1, 2, 3]
    ```

    ## Section 2

    Some markdown.

    <!-- livebook:{"force_markdown":true} -->

    ```elixir
    [1, 2, 3]
    ```
    """

    document = Export.notebook_to_markdown(notebook)

    assert expected_document == document
  end

  test "formats code in Elixir cells" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        metadata: %{},
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              metadata: %{},
              cells: [
                %{
                  Notebook.Cell.new(:elixir)
                  | metadata: %{},
                    source: """
                    [1,2,3] # Comment
                    """
                }
              ]
          }
        ]
    }

    expected_document = """
    # My Notebook

    ## Section 1

    ```elixir
    # Comment
    [1, 2, 3]
    ```
    """

    document = Export.notebook_to_markdown(notebook)

    assert expected_document == document
  end

  test "does not format code in Elixir cells which explicitly state so in metadata" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        metadata: %{},
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              metadata: %{},
              cells: [
                %{
                  Notebook.Cell.new(:elixir)
                  | metadata: %{"disable_formatting" => true},
                    source: """
                    [1,2,3] # Comment\
                    """
                }
              ]
          }
        ]
    }

    expected_document = """
    # My Notebook

    ## Section 1

    <!-- livebook:{"disable_formatting":true} -->

    ```elixir
    [1,2,3] # Comment
    ```
    """

    document = Export.notebook_to_markdown(notebook)

    assert expected_document == document
  end
end

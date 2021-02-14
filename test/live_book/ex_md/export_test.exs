defmodule LiveBook.ExMd.ExportTest do
  use ExUnit.Case, async: true

  alias LiveBook.ExMd.Export
  alias LiveBook.Notebook

  test "acceptance" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        metadata: %{author: "Sherlock Holmes"},
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              metadata: %{created_at: ~D/2021-02-15/},
              cells: [
                %{
                  Notebook.Cell.new(:markdown)
                  | metadata: %{updated_at: ~D/2021-02-15/},
                    source: """
                    Make sure to install:

                    * Erlang
                    * Elixir
                    * PostgreSQL\
                    """
                },
                %{
                  Notebook.Cell.new(:elixir)
                  | metadata: %{readonly: true},
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
                  Notebook.Cell.new(:elixir)
                  | metadata: %{},
                    source: """
                    # More Elixir code\
                    """
                }
              ]
          }
        ]
    }

    expected_document = """
    <!--live_book:author:"Sherlock Holmes"-->
    # My Notebook

    <!--live_book:created_at:"2021-02-15"-->
    ## Section 1

    <!--live_book:updated_at:"2021-02-15"-->
    Make sure to install:

    * Erlang
    * Elixir
    * PostgreSQL

    <!--live_book:readonly:true-->
    ```elixir
    Enum.to_list(1..10)
    ```

    This is it for this section.

    ## Section 2

    ```elixir
    # More Elixir code
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
end

defmodule LiveBook.ExMd.ImportTest do
  use ExUnit.Case, async: true

  alias LiveBook.ExMd.Import
  alias LiveBook.Notebook

  test "acceptance" do
    markdown = """
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

    notebook = Import.notebook_from_markdown(markdown)

    # Match only on the relevant fields as some may be generated (ids).

    assert %Notebook{
             name: "My Notebook",
             metadata: %{author: "Sherlock Holmes"},
             sections: [
               %Notebook.Section{
                 name: "Section 1",
                 metadata: %{created_at: "2021-02-15"},
                 cells: [
                   %Notebook.Cell{
                     type: :markdown,
                     metadata: %{updated_at: "2021-02-15"},
                     source: """
                     Make sure to install:

                     * Erlang
                     * Elixir
                     * PostgreSQL\
                     """
                   },
                   %Notebook.Cell{
                     type: :elixir,
                     metadata: %{readonly: true},
                     source: """
                     Enum.to_list(1..10)\
                     """
                   },
                   %Notebook.Cell{
                     type: :markdown,
                     metadata: %{},
                     source: """
                     This is it for this section.\
                     """
                   }
                 ]
               },
               %Notebook.Section{
                 name: "Section 2",
                 metadata: %{},
                 cells: [
                   %Notebook.Cell{
                     metadata: %{},
                     source: """
                     # More Elixir code\
                     """
                   }
                 ]
               }
             ]
           } = notebook
  end

  test "reformats markdown cells" do
    markdown = """
    # My Notebook

    ## Section 1

    |State|Abbrev|Capital|
    | --: | :-: | --- |
    | Texas | TX | Austin |
    | Maine | ME | Augusta |
    """

    notebook = Import.notebook_from_markdown(markdown)

    assert %Notebook{
             name: "My Notebook",
             sections: [
               %Notebook.Section{
                 name: "Section 1",
                 metadata: %{},
                 cells: [
                   %Notebook.Cell{
                     type: :markdown,
                     metadata: %{},
                     source: """
                     | State | Abbrev | Capital |
                     | ----: | :----: | ------- |
                     | Texas | TX     | Austin  |
                     | Maine | ME     | Augusta |\
                     """
                   }
                 ]
               }
             ]
           } = notebook
  end

  test "uses default name if there is no primary heading" do
    markdown = """
    ## Section 1

    Some markdown.
    """

    notebook = Import.notebook_from_markdown(markdown)

    assert %Notebook{
             name: "Untitled notebook",
             sections: [
               %Notebook.Section{
                 name: "Section 1"
               }
             ]
           } = notebook
  end
end

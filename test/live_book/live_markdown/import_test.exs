defmodule LiveBook.LiveMarkdown.ImportTest do
  use ExUnit.Case, async: true

  alias LiveBook.LiveMarkdown.Import
  alias LiveBook.Notebook

  test "acceptance" do
    markdown = """
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

    ```elixir
    # More Elixir code
    ```
    """

    {notebook, []} = Import.notebook_from_markdown(markdown)

    # Match only on the relevant fields as some may be generated (ids).

    assert %Notebook{
             name: "My Notebook",
             metadata: %{"author" => "Sherlock Holmes"},
             sections: [
               %Notebook.Section{
                 name: "Section 1",
                 metadata: %{"created_at" => "2021-02-15"},
                 cells: [
                   %Notebook.Cell{
                     type: :markdown,
                     metadata: %{"updated_at" => "2021-02-15"},
                     source: """
                     Make sure to install:

                     * Erlang
                     * Elixir
                     * PostgreSQL\
                     """
                   },
                   %Notebook.Cell{
                     type: :elixir,
                     metadata: %{"readonly" => true},
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

    {notebook, []} = Import.notebook_from_markdown(markdown)

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

    {notebook, []} = Import.notebook_from_markdown(markdown)

    assert %Notebook{
             name: "Untitled notebook",
             sections: [
               %Notebook.Section{
                 name: "Section 1"
               }
             ]
           } = notebook
  end

  test "given multiple primary heading, downgrades all headings" do
    markdown = """
    # Probably section 1

    ## Heading

    Some markdown.

    # Probably section 2

    ###### Tiny heading
    """

    {notebook, messages} = Import.notebook_from_markdown(markdown)

    assert %Notebook{
             name: "Untitled notebook",
             sections: [
               %Notebook.Section{
                 name: "Probably section 1",
                 cells: [
                   %Notebook.Cell{
                     type: :markdown,
                     metadata: %{},
                     source: """
                     ### Heading

                     Some markdown.\
                     """
                   }
                 ]
               },
               %Notebook.Section{
                 name: "Probably section 2",
                 cells: [
                   %Notebook.Cell{
                     type: :markdown,
                     metadata: %{},
                     source: """
                     **Tiny heading**\
                     """
                   }
                 ]
               }
             ]
           } = notebook

    assert ["Downgrading all headings, because 2 instances of heading 1 were found"] == messages
  end

  test "ignores markdown modifiers in notebok/section names" do
    markdown = """
    # My *Notebook*

    ## [Section 1](https://example.com)
    """

    {notebook, []} = Import.notebook_from_markdown(markdown)

    assert %Notebook{
             name: "My Notebook",
             sections: [
               %Notebook.Section{
                 name: "Section 1"
               }
             ]
           } = notebook
  end

  test "adds a default section if there is some section-less content" do
    markdown = """
    # My Notebook

    Some markdown.

    ## Actual section
    """

    {notebook, []} = Import.notebook_from_markdown(markdown)

    assert %Notebook{
             name: "My Notebook",
             sections: [
               %Notebook.Section{
                 name: "Section",
                 cells: [
                   %Notebook.Cell{
                     type: :markdown,
                     source: """
                     Some markdown.\
                     """
                   }
                 ]
               },
               %Notebook.Section{
                 name: "Actual section"
               }
             ]
           } = notebook
  end

  test "uses defaults if there are no headings" do
    markdown = """
    ```elixir
    Enum.to_list(1..10)
    ```
    """

    {notebook, []} = Import.notebook_from_markdown(markdown)

    assert %Notebook{
             name: "Untitled notebook",
             sections: [
               %Notebook.Section{
                 name: "Section",
                 cells: [
                   %Notebook.Cell{
                     type: :elixir,
                     source: """
                     Enum.to_list(1..10)\
                     """
                   }
                 ]
               }
             ]
           } = notebook
  end

  test "moves the primary heading and preceding comments to the top" do
    markdown = """
    Cool notebook.

    <!-- livebook:{"author":"Sherlock Holmes"} -->

    # My Notebook

    Some markdown.
    """

    {notebook, messages} = Import.notebook_from_markdown(markdown)

    assert %Notebook{
             name: "My Notebook",
             metadata: %{"author" => "Sherlock Holmes"},
             sections: [
               %Notebook.Section{
                 name: "Section",
                 cells: [
                   %Notebook.Cell{
                     type: :markdown,
                     source: """
                     Cool notebook.

                     Some markdown.\
                     """
                   }
                 ]
               }
             ]
           } = notebook

    assert ["Moving heading 1 to the top of the notebook"] == messages
  end

  test "includes parsing warnings in the returned message list" do
    markdown = """
    # My notebook

    `

    Some markdown.
    """

    {_notebook, messages} = Import.notebook_from_markdown(markdown)

    assert ["Line 3: Closing unclosed backquotes ` at end of input"] == messages
  end

  test "imports non-elixir code snippets as part of markdown cells" do
    markdown = """
    # My Notebook

    ## Section 1

    ```shell
    mix deps.get
    ```

    ```elixir
    Enum.to_list(1..10)
    ```

    ```erlang
    spawn_link(fun() -> io:format("Hiya") end).
    ```
    """

    {notebook, []} = Import.notebook_from_markdown(markdown)

    assert %Notebook{
             name: "My Notebook",
             sections: [
               %Notebook.Section{
                 name: "Section 1",
                 cells: [
                   %Notebook.Cell{
                     type: :markdown,
                     source: """
                     ```shell
                     mix deps.get
                     ```\
                     """
                   },
                   %Notebook.Cell{
                     type: :elixir,
                     source: """
                     Enum.to_list(1..10)\
                     """
                   },
                   %Notebook.Cell{
                     type: :markdown,
                     source: """
                     ```erlang
                     spawn_link(fun() -> io:format("Hiya") end).
                     ```\
                     """
                   }
                 ]
               }
             ]
           } = notebook
  end

  test "imports elixir snippets as part of markdown cells if marked as such" do
    markdown = """
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

    {notebook, []} = Import.notebook_from_markdown(markdown)

    assert %Notebook{
             name: "My Notebook",
             sections: [
               %Notebook.Section{
                 name: "Section 1",
                 cells: [
                   %Notebook.Cell{
                     type: :markdown,
                     source: """
                     ```elixir
                     [1, 2, 3]
                     ```\
                     """
                   }
                 ]
               },
               %Notebook.Section{
                 name: "Section 2",
                 cells: [
                   %Notebook.Cell{
                     type: :markdown,
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
           } = notebook
  end
end

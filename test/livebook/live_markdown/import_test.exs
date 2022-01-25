defmodule Livebook.LiveMarkdown.ImportTest do
  use ExUnit.Case, async: true

  alias Livebook.LiveMarkdown.Import
  alias Livebook.Notebook
  alias Livebook.Notebook.Cell

  test "acceptance" do
    markdown = """
    # My Notebook

    ## Section 1

    Make sure to install:

    * Erlang
    * Elixir
    * PostgreSQL

    $x_{i} + y_{i}$

    <!-- livebook:{"disable_formatting":true,"reevaluate_automatically":true} -->

    ```elixir
    Enum.to_list(1..10)
    ```

    This is it for this section.

    ## Section 2

    ```elixir
    IO.gets("length: ")
    ```

    <!-- livebook:{"branch_parent_index":1} -->

    ## Section 3

    ```elixir
    Process.info()
    ```
    """

    {notebook, []} = Import.notebook_from_markdown(markdown)

    # Match only on the relevant fields as some may be generated (ids).

    assert %Notebook{
             name: "My Notebook",
             sections: [
               %Notebook.Section{
                 name: "Section 1",
                 cells: [
                   %Cell.Markdown{
                     source: """
                     Make sure to install:

                     * Erlang
                     * Elixir
                     * PostgreSQL

                     $x_{i} + y_{i}$\
                     """
                   },
                   %Cell.Elixir{
                     disable_formatting: true,
                     reevaluate_automatically: true,
                     source: """
                     Enum.to_list(1..10)\
                     """
                   },
                   %Cell.Markdown{
                     source: """
                     This is it for this section.\
                     """
                   }
                 ]
               },
               %Notebook.Section{
                 id: section2_id,
                 name: "Section 2",
                 cells: [
                   %Cell.Elixir{
                     source: """
                     IO.gets("length: ")\
                     """
                   }
                 ]
               },
               %Notebook.Section{
                 name: "Section 3",
                 parent_id: section2_id,
                 cells: [
                   %Cell.Elixir{
                     source: """
                     Process.info()\
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
                 cells: [
                   %Cell.Markdown{
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
                   %Cell.Markdown{
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
                   %Cell.Markdown{
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

  test "preserves markdown modifiers in notebok/section names" do
    markdown = """
    # My *Notebook*

    ## [Section 1](https://example.com)

    ## ---

    ## # Section
    """

    {notebook, []} = Import.notebook_from_markdown(markdown)

    assert %Notebook{
             name: "My *Notebook*",
             sections: [
               %Notebook.Section{name: "[Section 1](https://example.com)"},
               %Notebook.Section{name: "---"},
               %Notebook.Section{name: "# Section"}
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
                   %Cell.Markdown{
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
                   %Cell.Elixir{
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
             sections: [
               %Notebook.Section{
                 name: "Section",
                 cells: [
                   %Cell.Markdown{
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

    <!-- livebook:{"force_markdown":true} -->

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
                   %Cell.Markdown{
                     source: """
                     ```shell
                     mix deps.get
                     ```\
                     """
                   },
                   %Cell.Elixir{
                     source: """
                     Enum.to_list(1..10)\
                     """
                   },
                   %Cell.Markdown{
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
                   %Cell.Markdown{
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
                   %Cell.Markdown{
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

  test "imports markdown content into separate cells when a break annotation is encountered" do
    markdown = """
    # My Notebook

    ## Section 1

    Cell 1

    <!-- livebook:{"break_markdown":true} -->

    Cell 2
    """

    {notebook, []} = Import.notebook_from_markdown(markdown)

    assert %Notebook{
             name: "My Notebook",
             sections: [
               %Notebook.Section{
                 name: "Section 1",
                 cells: [
                   %Cell.Markdown{
                     source: """
                     Cell 1\
                     """
                   },
                   %Cell.Markdown{
                     source: """
                     Cell 2\
                     """
                   }
                 ]
               }
             ]
           } = notebook
  end

  test "imports comments preceding the notebook title" do
    markdown = """
    <!-- vim: set syntax=markdown: -->

    <!--nowhitespace-->

    <!--
      Multi
      line
    -->

    <!-- livebook:{"persist_outputs":true} -->

    # My Notebook

    ## Section 1

    Cell 1
    """

    {notebook, []} = Import.notebook_from_markdown(markdown)

    assert %Notebook{
             name: "My Notebook",
             persist_outputs: true,
             leading_comments: [
               ["vim: set syntax=markdown:"],
               ["nowhitespace"],
               ["  Multi", "  line"]
             ],
             sections: [
               %Notebook.Section{
                 name: "Section 1",
                 cells: [
                   %Cell.Markdown{
                     source: """
                     Cell 1\
                     """
                   }
                 ]
               }
             ]
           } = notebook
  end

  test "warns when custom comments are mixed with notebook metadata" do
    markdown = """
    <!-- livebook:{"persist_outputs":true} -->

    <!-- vim: set syntax=markdown: -->

    # My Notebook

    ## Section 1

    Cell 1
    """

    {notebook,
     [
       "found an invalid sequence of comments at the beginning, make sure custom comments are at the very top"
     ]} = Import.notebook_from_markdown(markdown)

    assert %Notebook{
             name: "My Notebook",
             persist_outputs: false,
             leading_comments: [],
             sections: [
               %Notebook.Section{
                 name: "Section 1",
                 cells: [
                   %Cell.Markdown{
                     source: """
                     Cell 1\
                     """
                   }
                 ]
               }
             ]
           } = notebook
  end

  describe "outputs" do
    test "imports output snippets as cell textual outputs" do
      markdown = """
      # My Notebook

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"output":true} -->

      ```
      hey
      ```

      <!-- livebook:{"output":true} -->

      ```
      :ok
      ```
      """

      {notebook, []} = Import.notebook_from_markdown(markdown)

      assert %Notebook{
               name: "My Notebook",
               sections: [
                 %Notebook.Section{
                   name: "Section 1",
                   cells: [
                     %Cell.Elixir{
                       source: """
                       IO.puts("hey")\
                       """,
                       outputs: [{0, {:text, ":ok"}}, {1, {:text, "hey"}}]
                     }
                   ]
                 }
               ],
               output_counter: 2
             } = notebook
    end

    test "discards other output snippets" do
      markdown = """
      # My Notebook

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      ```elixir
      plot()
      ```

      <!-- livebook:{"output":true} -->

      ```vega-lite
      {}
      ```

      ```elixir
      :ok
      ```
      """

      {notebook, []} = Import.notebook_from_markdown(markdown)

      assert %Notebook{
               name: "My Notebook",
               sections: [
                 %Notebook.Section{
                   name: "Section 1",
                   cells: [
                     %Cell.Elixir{
                       source: """
                       IO.puts("hey")\
                       """,
                       outputs: []
                     },
                     %Cell.Elixir{
                       source: """
                       plot()\
                       """,
                       outputs: []
                     },
                     %Cell.Elixir{
                       source: """
                       :ok\
                       """,
                       outputs: []
                     }
                   ]
                 }
               ]
             } = notebook
    end

    test "imports notebook :persist_outputs attribute" do
      markdown = """
      <!-- livebook:{"persist_outputs":true} -->

      # My Notebook
      """

      {notebook, []} = Import.notebook_from_markdown(markdown)

      assert %Notebook{name: "My Notebook", persist_outputs: true} = notebook
    end
  end

  test "imports notebook :autosave_interval_s attribute" do
    markdown = """
    <!-- livebook:{"autosave_interval_s":10} -->

    # My Notebook
    """

    {notebook, []} = Import.notebook_from_markdown(markdown)

    assert %Notebook{name: "My Notebook", autosave_interval_s: 10} = notebook
  end

  describe "backward compatibility" do
    test "warns if the imported notebook includes an input" do
      markdown = """
      # My Notebook

      ## Section 1

      <!-- livebook:{"livebook_object":"cell_input","name":"length","type":"text","value":"100"} -->
      """

      {_notebook, messages} = Import.notebook_from_markdown(markdown)

      assert [
               "found an input cell, but those are no longer supported, please use Kino.Input instead"
             ] == messages
    end

    test "warns if the imported notebook includes a reactive input" do
      markdown = """
      # My Notebook

      ## Section 1

      <!-- livebook:{"livebook_object":"cell_input","name":"length","reactive":true,"type":"text","value":"100"} -->
      """

      {_notebook, messages} = Import.notebook_from_markdown(markdown)

      assert [
               "found an input cell, but those are no longer supported, please use Kino.Input instead." <>
                 " Also, to make the input reactive you can use an automatically reevaluating cell"
             ] == messages
    end

    test "imports snippets with output info string" do
      # We now explicitly mark every output sinppet with <!-- livebook:{"output":true} -->
      # and use empty snippets for textual outputs, however previously
      # we supported ```output too, so let's ensure they still work

      markdown = """
      # My Notebook

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      ```output
      hey
      ```

      ```output
      :ok
      ```
      """

      {notebook, []} = Import.notebook_from_markdown(markdown)

      assert %Notebook{
               name: "My Notebook",
               sections: [
                 %Notebook.Section{
                   name: "Section 1",
                   cells: [
                     %Cell.Elixir{
                       source: """
                       IO.puts("hey")\
                       """,
                       outputs: [{0, {:text, ":ok"}}, {1, {:text, "hey"}}]
                     }
                   ]
                 }
               ],
               output_counter: 2
             } = notebook
    end
  end
end

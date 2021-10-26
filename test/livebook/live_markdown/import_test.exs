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

    <!-- livebook:{"livebook_object":"cell_input","name":"length","reactive":true,"type":"text","value":"100"} -->

    ```elixir
    IO.gets("length: ")
    ```

    <!-- livebook:{"livebook_object":"cell_input","name":"length","props":{"max":150,"min":50,"step":2},"type":"range","value":"100"} -->

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
                   %Cell.Input{
                     type: :text,
                     name: "length",
                     value: "100",
                     reactive: true
                   },
                   %Cell.Elixir{
                     source: """
                     IO.gets("length: ")\
                     """
                   },
                   %Cell.Input{
                     type: :range,
                     name: "length",
                     value: "100",
                     props: %{min: 50, max: 150, step: 2}
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

  test "sets default input types props if not provided" do
    markdown = """
    # My Notebook

    ## Section 1

    <!-- livebook:{"livebook_object":"cell_input","name":"length","props":{"extra":100,"max":150},"type":"range","value":"100"} -->
    """

    {notebook, []} = Import.notebook_from_markdown(markdown)

    expected_props = %{min: 0, max: 150, step: 1}

    assert %Notebook{
             name: "My Notebook",
             sections: [
               %Notebook.Section{
                 name: "Section 1",
                 cells: [
                   %Cell.Input{
                     type: :range,
                     props: ^expected_props
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

  describe "outputs" do
    test "imports output snippets as cell textual outputs" do
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
                       outputs: [{:text, ":ok"}, {:text, "hey"}]
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

  test "skips invalid input type and returns a message" do
    markdown = """
    # My Notebook

    ## Section 1

    <!-- livebook:{"livebook_object":"cell_input","type":"input_from_the_future"} -->
    """

    {_notebook, messages} = Import.notebook_from_markdown(markdown)

    assert [
             ~s{unrecognised input type "input_from_the_future", if it's a valid type it means your Livebook version doesn't support it}
           ] == messages
  end
end

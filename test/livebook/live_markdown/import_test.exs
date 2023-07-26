defmodule Livebook.LiveMarkdown.ImportTest do
  use ExUnit.Case, async: true

  import Livebook.TestHelpers

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

    <!-- livebook:{"continue_on_error":true,"disable_formatting":true,"reevaluate_automatically":true} -->

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

    <!-- livebook:{"attrs":{"text":"My text"},"livebook_object":"smart_cell","kind":"text"} -->

    ```elixir
    IO.puts("My text")
    ```

    <!-- livebook:{"attrs":{},"chunks":[[0,5],[7,5]],"kind":"multi_chunk","livebook_object":"smart_cell"} -->

    ```elixir
    x = 1

    x * x
    ```

    ```erlang
    lists:seq(1, 10).
    ```
    """

    {notebook, []} = Import.notebook_from_livemd(markdown)

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
                   %Cell.Code{
                     disable_formatting: true,
                     reevaluate_automatically: true,
                     continue_on_error: true,
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
                   %Cell.Code{
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
                   %Cell.Code{
                     source: """
                     Process.info()\
                     """
                   },
                   %Cell.Smart{
                     source: """
                     IO.puts("My text")\
                     """,
                     attrs: %{"text" => "My text"},
                     kind: "text"
                   },
                   %Cell.Smart{
                     source: """
                     x = 1

                     x * x\
                     """,
                     attrs: %{},
                     chunks: [{0, 5}, {7, 5}],
                     kind: "multi_chunk"
                   },
                   %Cell.Code{
                     language: :erlang,
                     source: """
                     lists:seq(1, 10).\
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

    {notebook, []} = Import.notebook_from_livemd(markdown)

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

    {notebook, []} = Import.notebook_from_livemd(markdown)

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

    {notebook, messages} = Import.notebook_from_livemd(markdown)

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

    assert ["downgrading all headings, because 2 instances of heading 1 were found"] == messages
  end

  test "preserves markdown modifiers in notebok/section names" do
    markdown = """
    # My *Notebook*

    ## [Section 1](https://example.com)

    ## ---

    ## # Section
    """

    {notebook, []} = Import.notebook_from_livemd(markdown)

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

    {notebook, []} = Import.notebook_from_livemd(markdown)

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

    {notebook, []} = Import.notebook_from_livemd(markdown)

    assert %Notebook{
             name: "Untitled notebook",
             sections: [
               %Notebook.Section{
                 name: "Section",
                 cells: [
                   %Cell.Code{
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

    {notebook, messages} = Import.notebook_from_livemd(markdown)

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

    assert ["moving heading 1 to the top of the notebook"] == messages
  end

  test "includes parsing warnings in the returned message list" do
    markdown = """
    # My notebook

    `

    Some markdown.
    """

    {_notebook, messages} = Import.notebook_from_livemd(markdown)

    assert ["line 3 - closing unclosed backquotes ` at end of input"] == messages
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

    {notebook, []} = Import.notebook_from_livemd(markdown)

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
                   %Cell.Code{
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

    {notebook, []} = Import.notebook_from_livemd(markdown)

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

    {notebook, []} = Import.notebook_from_livemd(markdown)

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

    {notebook, []} = Import.notebook_from_livemd(markdown)

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

    {notebook, messages} = Import.notebook_from_livemd(markdown)

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

    assert messages == [
             "found an invalid sequence of comments at the beginning, make sure custom comments are at the very top"
           ]
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

      {notebook, []} = Import.notebook_from_livemd(markdown)

      assert %Notebook{
               name: "My Notebook",
               sections: [
                 %Notebook.Section{
                   name: "Section 1",
                   cells: [
                     %Cell.Code{
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

      {notebook, []} = Import.notebook_from_livemd(markdown)

      assert %Notebook{
               name: "My Notebook",
               sections: [
                 %Notebook.Section{
                   name: "Section 1",
                   cells: [
                     %Cell.Code{
                       source: """
                       IO.puts("hey")\
                       """,
                       outputs: []
                     },
                     %Cell.Code{
                       source: """
                       plot()\
                       """,
                       outputs: []
                     },
                     %Cell.Code{
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

      {notebook, []} = Import.notebook_from_livemd(markdown)

      assert %Notebook{name: "My Notebook", persist_outputs: true} = notebook
    end
  end

  test "imports notebook :autosave_interval_s attribute" do
    markdown = """
    <!-- livebook:{"autosave_interval_s":10} -->

    # My Notebook
    """

    {notebook, []} = Import.notebook_from_livemd(markdown)

    assert %Notebook{name: "My Notebook", autosave_interval_s: 10} = notebook
  end

  test "imports notebook :default_language attribute" do
    markdown = """
    <!-- livebook:{"default_language":"erlang"} -->

    # My Notebook
    """

    {notebook, []} = Import.notebook_from_livemd(markdown)

    assert %Notebook{name: "My Notebook", default_language: :erlang} = notebook
  end

  test "imports notebook hub id when exists" do
    %{id: hub_id} = Livebook.Factory.insert_hub(:team)

    markdown = """
    <!-- livebook:{"hub_id":"#{hub_id}"} -->

    # My Notebook
    """

    {notebook, []} = Import.notebook_from_livemd(markdown)

    assert %Notebook{name: "My Notebook", hub_id: ^hub_id} = notebook
  end

  test "imports ignores hub id when does not exist" do
    markdown = """
    <!-- livebook:{"hub_id":"nonexistent"} -->

    # My Notebook
    """

    {notebook, messages} = Import.notebook_from_livemd(markdown)

    assert messages == ["ignoring notebook Hub with unknown id"]
    assert notebook.hub_id != "nonexistent"
  end

  describe "app settings" do
    test "imports settings" do
      markdown = """
      <!-- livebook:{"app_settings":{"access_type":"public","auto_shutdown_ms":5000,"multi_session":true,"output_type":"rich","show_existing_sessions":false,"show_source":true,"slug":"app"}} -->

      # My Notebook
      """

      {notebook, []} = Import.notebook_from_livemd(markdown)

      assert %Notebook{
               name: "My Notebook",
               app_settings: %{
                 slug: "app",
                 multi_session: true,
                 zero_downtime: false,
                 show_existing_sessions: false,
                 auto_shutdown_ms: 5_000,
                 access_type: :public,
                 show_source: true,
                 output_type: :rich
               }
             } = notebook
    end

    test "correctly imports protected access" do
      markdown = """
      <!-- livebook:{"app_settings":{"slug":"app"}} -->

      # My Notebook
      """

      {notebook, []} = Import.notebook_from_livemd(markdown)

      assert %Notebook{
               name: "My Notebook",
               app_settings: %{slug: "app", access_type: :protected}
             } = notebook
    end
  end

  describe "backward compatibility" do
    test "warns if the imported notebook includes an input" do
      markdown = """
      # My Notebook

      ## Section 1

      <!-- livebook:{"livebook_object":"cell_input","name":"length","type":"text","value":"100"} -->
      """

      {_notebook, messages} = Import.notebook_from_livemd(markdown)

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

      {_notebook, messages} = Import.notebook_from_livemd(markdown)

      assert [
               "found an input cell, but those are no longer supported, please use Kino.Input instead." <>
                 " Also, to make the input reactive you can use an automatically reevaluating cell"
             ] == messages
    end

    test "imports snippets with output info string" do
      # We now explicitly mark every output snippet with <!-- livebook:{"output":true} -->
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

      {notebook, []} = Import.notebook_from_livemd(markdown)

      assert %Notebook{
               name: "My Notebook",
               sections: [
                 %Notebook.Section{
                   name: "Section 1",
                   cells: [
                     %Cell.Code{
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

    test "warns if the imported notebook includes images pointing to images/ directory" do
      markdown = """
      # My Notebook

      ## Section 1

      ![](images/cat.jpeg)

      ![](images/dog.jpeg)
      """

      {_notebook, messages} = Import.notebook_from_livemd(markdown)

      assert [
               "found Markdown images pointing to the images/ directory. Using this directory has been deprecated, please use notebook files instead"
             ] == messages
    end
  end

  test "import notebook with invalid parent section produces a warning" do
    markdown = """
    # My Notebook

    <!-- livebook:{"branch_parent_index":4} -->

    ## Section 1

    ```elixir
    Process.info()
    ```

    ## Section 2

    ```elixir
    Process.info()
    ```
    """

    {notebook, messages} = Import.notebook_from_livemd(markdown)

    assert %Notebook{
             name: "My Notebook",
             sections: [
               %Notebook.Section{
                 parent_id: nil
               },
               %Notebook.Section{
                 parent_id: nil
               }
             ]
           } = notebook

    assert messages == [
             "ignoring the parent section of \"Section 1\", because it comes later in the notebook"
           ]
  end

  test "import notebook with parent section pointing to the section itself produces a warning" do
    markdown = """
    # My Notebook

    <!-- livebook:{"branch_parent_index":0} -->

    ## Section 1

    ```elixir
    Process.info()
    ```
    """

    {notebook, messages} = Import.notebook_from_livemd(markdown)

    assert %Notebook{
             name: "My Notebook",
             sections: [
               %Notebook.Section{
                 parent_id: nil
               }
             ]
           } = notebook

    assert messages == [
             "ignoring the parent section of \"Section 1\", because it comes later in the notebook"
           ]
  end

  test "importing notebook with parent section being a branching section itself produces a warning" do
    markdown = """
    # My Notebook

    ## Section 1

    ```elixir
    Process.info()
    ```

    <!-- livebook:{"branch_parent_index":0} -->

    ## Section 2

    ```elixir
    Process.info()
    ```
    <!-- livebook:{"branch_parent_index":1} -->

    ## Section 3

    ```elixir
    Process.info()
    ```
    """

    {notebook, messages} = Import.notebook_from_livemd(markdown)

    assert %Notebook{
             name: "My Notebook",
             sections: [
               %Notebook.Section{
                 name: "Section 1",
                 parent_id: nil
               },
               %Notebook.Section{
                 name: "Section 2",
                 parent_id: _
               },
               %Notebook.Section{
                 name: "Section 3",
                 parent_id: nil
               }
             ]
           } = notebook

    assert messages == [
             "ignoring the parent section of \"Section 3\", because it is itself a branching section"
           ]
  end

  describe "setup cell" do
    test "imports a leading setup cell" do
      markdown = """
      # My Notebook

      ```elixir
      Mix.install([...])
      ```

      ## Section 1
      """

      {notebook, []} = Import.notebook_from_livemd(markdown)

      assert %Notebook{
               name: "My Notebook",
               setup_section: %{
                 cells: [
                   %Cell.Code{id: "setup", source: "Mix.install([...])"}
                 ]
               },
               sections: [
                 %Notebook.Section{
                   name: "Section 1",
                   cells: []
                 }
               ]
             } = notebook
    end

    test "does not add an implicit section when there is just setup cell" do
      markdown = """
      # My Notebook

      ```elixir
      Mix.install([...])
      ```
      """

      {notebook, []} = Import.notebook_from_livemd(markdown)

      assert %Notebook{
               name: "My Notebook",
               setup_section: %{
                 cells: [
                   %Cell.Code{id: "setup", source: "Mix.install([...])"}
                 ]
               },
               sections: []
             } = notebook
    end
  end

  describe "notebook stamp" do
    test "restores hub secret names from notebook stamp" do
      # Generated with:

      # %{
      #   Notebook.new()
      #   | name: "My Notebook",
      #     sections: [
      #       %{
      #         Notebook.Section.new()
      #         | name: "Section 1",
      #           cells: [
      #             %{
      #               Notebook.Cell.new(:code)
      #               | source: """
      #                 IO.puts("hey")
      #                 """
      #             }
      #           ]
      #       }
      #     ],
      #     hub_secret_names: ["DB_PASSWORD"]
      # }
      # |> Livebook.LiveMarkdown.Export.notebook_to_livemd()
      # |> elem(0)
      # |> IO.puts()

      markdown = """
      # My Notebook

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"offset":58,"stamp":{"token":"QTEyOEdDTQ.LF8LTeMYrtq8S7wsKMmk2YgOQzMAkEKT2d8fq1Gz3Ot1mydOgEZ1B4hcEZc.Wec6NwBQ584kE661.a_N-5jDiWrjhHha9zxHQ6JJOmxeqgiya3m6YlKt1Na_DPnEfXyLnengaUzQSrf8.ZoD5r6-H87RpTyvFkvEOQw","version":1}} -->
      """

      {notebook, []} = Import.notebook_from_livemd(markdown)

      assert %Notebook{hub_secret_names: ["DB_PASSWORD"]} = notebook
    end

    test "returns a warning when notebook stamp is invalid" do
      markdown = """
      # My Notebook

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"offset":58,"stamp":{"token":"invalid","version":1}} -->
      """

      {notebook, messages} = Import.notebook_from_livemd(markdown)

      assert %Notebook{hub_secret_names: []} = notebook
      assert messages == ["failed to verify notebook stamp"]
    end

    test "restores hub secret names from notebook stamp using offline hub" do
      markdown = """
      <!-- livebook:{"hub_id":"team-org-number-3079"} -->

      # My Notebook

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"offset":111,"stamp":{"token":"QTEyOEdDTQ.VezIVId8jnwawq79Mrxfx-bCeatRPQhg6zRw4gHjfGC_SimS5WSbdETLVnc.IXXzTiDf3JRLiNGt.rtLJ97RxNYLeF_x-2AigBt_y0bYdlh30bnEKUO7FKfJL-9EB6EQOgr1WbdHTkw.rU1OsjOhfU48tE3s0GB6Ag","token_signature":"M_LZP7ssEdLiVqo8_bZYFtJYAzqV5wczQF1mUdaOZeTYoCYXeLA9VuEajMCOwiVgMjfG24Y4PHrnABGUj0bB74xgEMOLtb6Ark42pCJfGUDLXpB2azMnwY6VRXi1nBUTnb_-MajpJVF_MgWGuj0cxyQ4rlfBu2VK1FQn8JGopLKBD7LcICZ9wkLJw3H_lXGTbVxnS6tLcvJZQdLJccTe4edpdF-_uG-d1KqtJva50jZ9vevyxh6zUubl0k5JnA3SUaTpc7AEsqmAMx9qQsoR64P-VobSFTKuuOFrlQsq21OupPCRKmUBU-RKrBvHbc6Fa1eoQigEQom5droj9cASdw","version":1}} -->
      """

      {notebook, []} = Import.notebook_from_livemd(markdown)

      assert %Notebook{
               hub_id: "team-org-number-3079",
               hub_secret_names: ["DB_PASSWORD"],
               teams_enabled: true
             } = notebook
    end

    test "returns a warning when notebook stamp is invalid using offline hub" do
      markdown = """
      <!-- livebook:{"hub_id":"team-org-number-3079"} -->

      # My Notebook

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"offset":58,"stamp":{"token":"invalid","token_signature":"invalid","version":1}} -->
      """

      {notebook, messages} = Import.notebook_from_livemd(markdown)

      assert %Notebook{hub_id: "team-org-number-3079", teams_enabled: true} = notebook
      assert messages == ["failed to verify notebook stamp"]
    end

    test "sets :teams_enabled to true when the teams hub exist regardless the stamp" do
      %{id: hub_id} = Livebook.Factory.insert_hub(:team)

      markdown = """
      <!-- livebook:{"hub_id":"#{hub_id}"} -->

      # My Notebook

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"offset":58,"stamp":{"token":"invalid","token_signature":"invalid","version":1}} -->
      """

      {notebook, [_]} = Import.notebook_from_livemd(markdown)

      assert %Notebook{hub_id: ^hub_id, teams_enabled: true} = notebook
    end
  end

  describe "file entries" do
    test "imports file entries" do
      markdown = """
      <!-- livebook:{"file_entries":[{"name":"data.csv","type":"url","url":"https://example.com/data.csv"},{"file":{"file_system_id":"local","path":"#{p("/document.pdf")}"},"name":"document.pdf","type":"file"},{"name":"image.jpg","type":"attachment"}]} -->

      # My Notebook
      """

      {notebook, []} = Import.notebook_from_livemd(markdown)

      assert %Notebook{
               file_entries: [
                 %{type: :attachment, name: "image.jpg"},
                 %{
                   type: :file,
                   name: "document.pdf",
                   file: %Livebook.FileSystem.File{
                     file_system: %Livebook.FileSystem.Local{},
                     path: p("/document.pdf")
                   }
                 },
                 %{type: :url, name: "data.csv", url: "https://example.com/data.csv"}
               ]
             } = notebook
    end

    test "skips file entries from unknown file system" do
      markdown = """
      <!-- livebook:{"file_entries":[{"file":{"file_system_id":"s3-nonexistent","path":"/document.pdf"},"name":"document.pdf","type":"file"}]} -->

      # My Notebook
      """

      {notebook, messages} = Import.notebook_from_livemd(markdown)

      assert %Notebook{file_entries: []} = notebook

      assert messages == ["skipping file document.pdf, since it points to an unknown file system"]
    end

    test "imports :file file entries with quarantine when no stamp is given" do
      markdown = """
      <!-- livebook:{"file_entries":[{"file":{"file_system_id":"local","path":"#{p("/document.pdf")}"},"name":"document.pdf","type":"file"}]} -->

      # My Notebook
      """

      {notebook, []} = Import.notebook_from_livemd(markdown)

      assert %Notebook{
               file_entries: [
                 %{
                   type: :file,
                   name: "document.pdf",
                   file: %Livebook.FileSystem.File{
                     file_system: %Livebook.FileSystem.Local{},
                     path: p("/document.pdf")
                   }
                 }
               ]
             } = notebook

      assert notebook.quarantine_file_entry_names == MapSet.new(["document.pdf"])
    end

    test "imports :file file entries with quarantine when the stamp is invalid" do
      file = Livebook.FileSystem.File.new(Livebook.FileSystem.Local.new(), p("/document.pdf"))

      # We generate the Live Markdown programmatically, because the
      # absolute path is a part of the stamp and it is different on
      # Windows
      {markdown, []} =
        %{
          Notebook.new()
          | name: "My Notebook",
            file_entries: [%{type: :file, name: "document.pdf", file: file}]
        }
        |> Livebook.LiveMarkdown.Export.notebook_to_livemd()

      # Change file path in the document
      markdown = String.replace(markdown, p("/document.pdf"), p("/other.pdf"))

      {notebook, _} = Import.notebook_from_livemd(markdown)

      assert %Notebook{
               file_entries: [
                 %{
                   type: :file,
                   name: "document.pdf",
                   file: %Livebook.FileSystem.File{
                     file_system: %Livebook.FileSystem.Local{},
                     path: p("/other.pdf")
                   }
                 }
               ]
             } = notebook

      assert notebook.quarantine_file_entry_names == MapSet.new(["document.pdf"])
    end

    test "imports quarantine file entry names from stamp metadata" do
      file = Livebook.FileSystem.File.new(Livebook.FileSystem.Local.new(), p("/document.pdf"))

      {markdown, []} =
        %{
          Notebook.new()
          | name: "My Notebook",
            file_entries: [
              %{type: :file, name: "document1.pdf", file: file},
              %{type: :file, name: "document2.pdf", file: file}
            ],
            quarantine_file_entry_names: MapSet.new(["document1.pdf"])
        }
        |> Livebook.LiveMarkdown.Export.notebook_to_livemd()

      {notebook, []} = Import.notebook_from_livemd(markdown)

      assert %Notebook{
               file_entries: [
                 %{
                   type: :file,
                   name: "document2.pdf",
                   file: %Livebook.FileSystem.File{
                     file_system: %Livebook.FileSystem.Local{},
                     path: p("/document.pdf")
                   }
                 },
                 %{
                   type: :file,
                   name: "document1.pdf",
                   file: %Livebook.FileSystem.File{
                     file_system: %Livebook.FileSystem.Local{},
                     path: p("/document.pdf")
                   }
                 }
               ]
             } = notebook

      assert notebook.quarantine_file_entry_names == MapSet.new(["document1.pdf"])
    end
  end
end

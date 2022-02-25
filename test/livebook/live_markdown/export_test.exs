defmodule Livebook.LiveMarkdown.ExportTest do
  use ExUnit.Case, async: true

  alias Livebook.LiveMarkdown.Export
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
                    * PostgreSQL

                    $x_{i} + y_{i}$\
                    """
                },
                %{
                  Notebook.Cell.new(:elixir)
                  | disable_formatting: true,
                    reevaluate_automatically: true,
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
                  Notebook.Cell.new(:elixir)
                  | source: """
                    IO.gets("length: ")\
                    """
                }
              ]
          },
          %{
            Notebook.Section.new()
            | name: "Section 3",
              parent_id: "s2",
              cells: [
                %{
                  Notebook.Cell.new(:elixir)
                  | source: """
                    Process.info()\
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
          }
        ]
    }

    expected_document = """
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

    <!-- livebook:{"attrs":{"text":"My text"},"kind":"text","livebook_object":"smart_cell"} -->

    ```elixir
    IO.puts("My text")
    ```
    """

    document = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  test "reformats markdown cells" do
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

    document = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  test "drops heading 1 and 2 in markdown cells" do
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

    document = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  test "keeps non-elixir code snippets" do
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

    document = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  test "marks elixir snippets in markdown cells as such" do
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
              cells: [
                %{
                  Notebook.Cell.new(:markdown)
                  | source: """
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

    document = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  test "formats code in Elixir cells" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              cells: [
                %{
                  Notebook.Cell.new(:elixir)
                  | source: """
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

    document = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  test "does not format code in Elixir cells which have formatting disabled" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              cells: [
                %{
                  Notebook.Cell.new(:elixir)
                  | disable_formatting: true,
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

    document = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  test "handles backticks in code cell" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              cells: [
                %{
                  Notebook.Cell.new(:elixir)
                  | source: """
                    \"\"\"
                    ```elixir
                    x = 1
                    ```

                    ````markdown
                    # Heading
                    ````
                    \"\"\"\
                    """
                }
              ]
          }
        ]
    }

    expected_document = """
    # My Notebook

    ## Section 1

    `````elixir
    \"\"\"
    ```elixir
    x = 1
    ```

    ````markdown
    # Heading
    ````
    \"\"\"
    `````
    """

    document = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  test "separates consecutive markdown cells by a break annotation" do
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
                    Cell 1\
                    """
                },
                %{
                  Notebook.Cell.new(:markdown)
                  | source: """
                    Cell 2\
                    """
                }
              ]
          }
        ]
    }

    expected_document = """
    # My Notebook

    ## Section 1

    Cell 1

    <!-- livebook:{"break_markdown":true} -->

    Cell 2
    """

    document = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  test "exports leading notebook comments" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        persist_outputs: true,
        leading_comments: [
          ["vim: set syntax=markdown:"],
          ["nowhitespace"],
          ["  Multi", "  line"]
        ],
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              cells: [
                %{
                  Notebook.Cell.new(:markdown)
                  | source: """
                    Cell 1\
                    """
                }
              ]
          }
        ]
    }

    expected_document = """
    <!-- vim: set syntax=markdown: -->

    <!-- nowhitespace -->

    <!--
      Multi
      line
    -->

    <!-- livebook:{"persist_outputs":true} -->

    # My Notebook

    ## Section 1

    Cell 1
    """

    document = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  describe "outputs" do
    test "does not include outputs by default" do
      notebook = %{
        Notebook.new()
        | name: "My Notebook",
          sections: [
            %{
              Notebook.Section.new()
              | name: "Section 1",
                cells: [
                  %{
                    Notebook.Cell.new(:elixir)
                    | source: """
                      IO.puts("hey")\
                      """,
                      outputs: [
                        {0, {:stdout, "hey"}}
                      ]
                  }
                ]
            }
          ]
      }

      expected_document = """
      # My Notebook

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```
      """

      document = Export.notebook_to_livemd(notebook)

      assert expected_document == document
    end

    test "includes outputs when :include_outputs option is set" do
      notebook = %{
        Notebook.new()
        | name: "My Notebook",
          sections: [
            %{
              Notebook.Section.new()
              | name: "Section 1",
                cells: [
                  %{
                    Notebook.Cell.new(:elixir)
                    | source: """
                      IO.puts("hey")\
                      """,
                      outputs: [{0, {:stdout, "hey"}}]
                  }
                ]
            }
          ]
      }

      expected_document = """
      # My Notebook

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"output":true} -->

      ```
      hey
      ```
      """

      document = Export.notebook_to_livemd(notebook, include_outputs: true)

      assert expected_document == document
    end

    test "removes ANSI escape codes from the output text" do
      notebook = %{
        Notebook.new()
        | name: "My Notebook",
          sections: [
            %{
              Notebook.Section.new()
              | name: "Section 1",
                cells: [
                  %{
                    Notebook.Cell.new(:elixir)
                    | source: """
                      IO.puts("hey")\
                      """,
                      outputs: [{0, {:text, "\e[34m:ok\e[0m"}}, {1, {:stdout, "hey"}}]
                  }
                ]
            }
          ]
      }

      expected_document = """
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

      document = Export.notebook_to_livemd(notebook, include_outputs: true)

      assert expected_document == document
    end

    test "ignores non-textual output types" do
      notebook = %{
        Notebook.new()
        | name: "My Notebook",
          sections: [
            %{
              Notebook.Section.new()
              | name: "Section 1",
                cells: [
                  %{
                    Notebook.Cell.new(:elixir)
                    | source: """
                      IO.puts("hey")\
                      """,
                      outputs: [{0, {:markdown, "some **Markdown**"}}]
                  }
                ]
            }
          ]
      }

      expected_document = """
      # My Notebook

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```
      """

      document = Export.notebook_to_livemd(notebook, include_outputs: true)

      assert expected_document == document
    end

    test "does not include js output with no export info" do
      notebook = %{
        Notebook.new()
        | name: "My Notebook",
          sections: [
            %{
              Notebook.Section.new()
              | name: "Section 1",
                cells: [
                  %{
                    Notebook.Cell.new(:elixir)
                    | source: ":ok",
                      outputs: [
                        {0,
                         {:js,
                          %{
                            js_view: %{
                              ref: "1",
                              pid: spawn_widget_with_data("1", "data"),
                              assets: %{archive_path: "", hash: "abcd", js_path: "main.js"}
                            },
                            export: nil
                          }}}
                      ]
                  }
                ]
            }
          ]
      }

      expected_document = """
      # My Notebook

      ## Section 1

      ```elixir
      :ok
      ```
      """

      document = Export.notebook_to_livemd(notebook, include_outputs: true)

      assert expected_document == document
    end

    test "includes js output if export info is set" do
      notebook = %{
        Notebook.new()
        | name: "My Notebook",
          sections: [
            %{
              Notebook.Section.new()
              | name: "Section 1",
                cells: [
                  %{
                    Notebook.Cell.new(:elixir)
                    | source: ":ok",
                      outputs: [
                        {0,
                         {:js,
                          %{
                            js_view: %{
                              ref: "1",
                              pid: spawn_widget_with_data("1", "graph TD;\nA-->B;"),
                              assets: %{archive_path: "", hash: "abcd", js_path: "main.js"}
                            },
                            export: %{info_string: "mermaid", key: nil}
                          }}}
                      ]
                  }
                ]
            }
          ]
      }

      expected_document = """
      # My Notebook

      ## Section 1

      ```elixir
      :ok
      ```

      <!-- livebook:{"output":true} -->

      ```mermaid
      graph TD;
      A-->B;
      ```
      """

      document = Export.notebook_to_livemd(notebook, include_outputs: true)

      assert expected_document == document
    end

    test "serializes js output data to JSON if not binary" do
      notebook = %{
        Notebook.new()
        | name: "My Notebook",
          sections: [
            %{
              Notebook.Section.new()
              | name: "Section 1",
                cells: [
                  %{
                    Notebook.Cell.new(:elixir)
                    | source: ":ok",
                      outputs: [
                        {0,
                         {:js,
                          %{
                            js_view: %{
                              ref: "1",
                              pid: spawn_widget_with_data("1", %{height: 50, width: 50}),
                              assets: %{archive_path: "", hash: "abcd", js_path: "main.js"}
                            },
                            export: %{info_string: "box", key: nil}
                          }}}
                      ]
                  }
                ]
            }
          ]
      }

      expected_document = """
      # My Notebook

      ## Section 1

      ```elixir
      :ok
      ```

      <!-- livebook:{"output":true} -->

      ```box
      {"height":50,"width":50}
      ```
      """

      document = Export.notebook_to_livemd(notebook, include_outputs: true)

      assert expected_document == document
    end

    test "exports partial js output data when export_key is set" do
      notebook = %{
        Notebook.new()
        | name: "My Notebook",
          sections: [
            %{
              Notebook.Section.new()
              | name: "Section 1",
                cells: [
                  %{
                    Notebook.Cell.new(:elixir)
                    | source: ":ok",
                      outputs: [
                        {0,
                         {:js,
                          %{
                            js_view: %{
                              ref: "1",
                              pid:
                                spawn_widget_with_data("1", %{
                                  spec: %{"height" => 50, "width" => 50},
                                  datasets: []
                                }),
                              assets: %{archive_path: "", hash: "abcd", js_path: "main.js"}
                            },
                            export: %{info_string: "vega-lite", key: :spec}
                          }}}
                      ]
                  }
                ]
            }
          ]
      }

      expected_document = """
      # My Notebook

      ## Section 1

      ```elixir
      :ok
      ```

      <!-- livebook:{"output":true} -->

      ```vega-lite
      {"height":50,"width":50}
      ```
      """

      document = Export.notebook_to_livemd(notebook, include_outputs: true)

      assert expected_document == document
    end
  end

  test "includes outputs when notebook has :persist_outputs set" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        persist_outputs: true,
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              cells: [
                %{
                  Notebook.Cell.new(:elixir)
                  | source: """
                    IO.puts("hey")\
                    """,
                    outputs: [{0, {:stdout, "hey"}}]
                }
              ]
          }
        ]
    }

    expected_document = """
    <!-- livebook:{"persist_outputs":true} -->

    # My Notebook

    ## Section 1

    ```elixir
    IO.puts("hey")
    ```

    <!-- livebook:{"output":true} -->

    ```
    hey
    ```
    """

    document = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  test "the :include_outputs option takes precedence over notebook's :persist_outputs" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        persist_outputs: true,
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              cells: [
                %{
                  Notebook.Cell.new(:elixir)
                  | source: """
                    IO.puts("hey")\
                    """,
                    outputs: [{0, {:stdout, "hey"}}]
                }
              ]
          }
        ]
    }

    expected_document = """
    <!-- livebook:{"persist_outputs":true} -->

    # My Notebook

    ## Section 1

    ```elixir
    IO.puts("hey")
    ```
    """

    document = Export.notebook_to_livemd(notebook, include_outputs: false)

    assert expected_document == document
  end

  test "persists :autosave_interval_s when other than default" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        autosave_interval_s: 10
    }

    expected_document = """
    <!-- livebook:{"autosave_interval_s":10} -->

    # My Notebook
    """

    document = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  defp spawn_widget_with_data(ref, data) do
    spawn(fn ->
      receive do
        {:connect, pid, %{ref: ^ref}} -> send(pid, {:connect_reply, data, %{ref: ref}})
      end
    end)
  end
end

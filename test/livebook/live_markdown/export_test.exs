defmodule Livebook.LiveMarkdown.ExportTest do
  use ExUnit.Case, async: true

  import Livebook.TestHelpers

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
                  Notebook.Cell.new(:code)
                  | disable_formatting: true,
                    reevaluate_automatically: true,
                    continue_on_error: true,
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
                  Notebook.Cell.new(:code)
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
                  Notebook.Cell.new(:code)
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
                },
                %{
                  Notebook.Cell.new(:smart)
                  | source: """
                    x = 1

                    x * x\
                    """,
                    chunks: [{0, 5}, {7, 5}],
                    kind: "multi_chunk"
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

    <!-- livebook:{"attrs":{"text":"My text"},"chunks":null,"kind":"text","livebook_object":"smart_cell"} -->

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

    {document, []} = Export.notebook_to_livemd(notebook)

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

    {document, []} = Export.notebook_to_livemd(notebook)

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

    {document, []} = Export.notebook_to_livemd(notebook)

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

    {document, []} = Export.notebook_to_livemd(notebook)

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

    {document, []} = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  test "formats code in code cells" do
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

    {document, []} = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  test "does not format code in code cells which have formatting disabled" do
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

    {document, []} = Export.notebook_to_livemd(notebook)

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
                  Notebook.Cell.new(:code)
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

    {document, []} = Export.notebook_to_livemd(notebook)

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

    {document, []} = Export.notebook_to_livemd(notebook)

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

    {document, []} = Export.notebook_to_livemd(notebook)

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
                    Notebook.Cell.new(:code)
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

      {document, []} = Export.notebook_to_livemd(notebook)

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
                    Notebook.Cell.new(:code)
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

      {document, []} = Export.notebook_to_livemd(notebook, include_outputs: true)

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
                    Notebook.Cell.new(:code)
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

      {document, []} = Export.notebook_to_livemd(notebook, include_outputs: true)

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
                    Notebook.Cell.new(:code)
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

      {document, []} = Export.notebook_to_livemd(notebook, include_outputs: true)

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
                    Notebook.Cell.new(:code)
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

      {document, []} = Export.notebook_to_livemd(notebook, include_outputs: true)

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
                    Notebook.Cell.new(:code)
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

      {document, []} = Export.notebook_to_livemd(notebook, include_outputs: true)

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
                    Notebook.Cell.new(:code)
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

      {document, []} = Export.notebook_to_livemd(notebook, include_outputs: true)

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
                    Notebook.Cell.new(:code)
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

      {document, []} = Export.notebook_to_livemd(notebook, include_outputs: true)

      assert expected_document == document
    end
  end

  test "includes only the first tabs output that can be exported" do
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
                  | source: ":ok",
                    outputs: [
                      {0,
                       {:tabs,
                        [
                          {1, {:markdown, "a"}},
                          {2, {:text, "b"}},
                          {3, {:text, "c"}}
                        ], %{labels: ["A", "B", "C"]}}}
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

    ```
    b
    ```
    """

    {document, []} = Export.notebook_to_livemd(notebook, include_outputs: true)

    assert expected_document == document
  end

  test "includes all grid outputs that can be exported" do
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
                  | source: ":ok",
                    outputs: [
                      {0,
                       {:grid,
                        [
                          {1, {:text, "a"}},
                          {2, {:markdown, "b"}},
                          {3, {:text, "c"}}
                        ], %{columns: 2}}}
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

    ```
    a
    ```

    <!-- livebook:{"output":true} -->

    ```
    c
    ```
    """

    {document, []} = Export.notebook_to_livemd(notebook, include_outputs: true)

    assert expected_document == document
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
                  Notebook.Cell.new(:code)
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

    {document, []} = Export.notebook_to_livemd(notebook)

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
                  Notebook.Cell.new(:code)
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

    {document, []} = Export.notebook_to_livemd(notebook, include_outputs: false)

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

    {document, []} = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  test "persists :default_language when other than default" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        default_language: :erlang
    }

    expected_document = """
    <!-- livebook:{"default_language":"erlang"} -->

    # My Notebook
    """

    {document, []} = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  test "persists hub id when not default" do
    %{id: hub_id} = Livebook.Factory.build(:team)

    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        hub_id: hub_id
    }

    expected_document = """
    <!-- livebook:{"hub_id":"#{hub_id}"} -->

    # My Notebook
    """

    {document, []} = Export.notebook_to_livemd(notebook)

    assert expected_document == document
  end

  describe "app settings" do
    test "persists non-default app settings" do
      notebook = %{
        Notebook.new()
        | name: "My Notebook",
          app_settings: %{
            Notebook.AppSettings.new()
            | slug: "app",
              multi_session: true,
              zero_downtime: false,
              show_existing_sessions: false,
              auto_shutdown_ms: 5_000,
              access_type: :public,
              show_source: true,
              output_type: :rich
          }
      }

      expected_document = """
      <!-- livebook:{"app_settings":{"access_type":"public","auto_shutdown_ms":5000,"multi_session":true,"output_type":"rich","show_existing_sessions":false,"show_source":true,"slug":"app"}} -->

      # My Notebook
      """

      {document, []} = Export.notebook_to_livemd(notebook)

      assert expected_document == document
    end

    test "does not persist password" do
      notebook = %{
        Notebook.new()
        | name: "My Notebook",
          app_settings: %{
            Notebook.AppSettings.new()
            | slug: "app",
              access_type: :protected,
              password: "verylongpass"
          }
      }

      expected_document = """
      <!-- livebook:{"app_settings":{"slug":"app"}} -->

      # My Notebook
      """

      {document, []} = Export.notebook_to_livemd(notebook)

      assert expected_document == document
    end
  end

  describe "setup cell" do
    test "includes the leading setup cell when it has content" do
      notebook =
        %{
          Notebook.new()
          | name: "My Notebook",
            sections: [%{Notebook.Section.new() | name: "Section 1"}]
        }
        |> Notebook.put_setup_cell(%{Notebook.Cell.new(:code) | source: "Mix.install([...])"})

      expected_document = """
      # My Notebook

      ```elixir
      Mix.install([...])
      ```

      ## Section 1
      """

      {document, []} = Export.notebook_to_livemd(notebook)

      assert expected_document == document
    end
  end

  describe "notebook stamp" do
    test "notebook stamp is appended at the end" do
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
                      IO.puts("hey")
                      """
                  }
                ]
            }
          ],
          hub_secret_names: ["DB_PASSWORD"]
      }

      expected_document = ~R"""
      # My Notebook

      ## Section 1

      ```elixir
      IO.puts\("hey"\)
      ```

      <!-- livebook:{"offset":58,"stamp":(.*)} -->
      """

      {document, []} = Export.notebook_to_livemd(notebook)

      assert document =~ expected_document
    end

    test "skips notebook stamp if :include_stamp is false" do
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
                      IO.puts("hey")
                      """
                  }
                ]
            }
          ],
          hub_secret_names: ["DB_PASSWORD"]
      }

      expected_document = """
      # My Notebook

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```
      """

      {document, []} = Export.notebook_to_livemd(notebook, include_stamp: false)

      assert expected_document == document
    end
  end

  describe "file entries" do
    test "persists file entries" do
      file = Livebook.FileSystem.File.new(Livebook.FileSystem.Local.new(), p("/document.pdf"))

      notebook = %{
        Notebook.new()
        | name: "My Notebook",
          file_entries: [
            %{type: :attachment, name: "image.jpg"},
            %{type: :url, name: "data.csv", url: "https://example.com/data.csv"},
            %{type: :file, name: "document.pdf", file: file}
          ]
      }

      expected_document = """
      <!-- livebook:{"file_entries":[{"name":"data.csv","type":"url","url":"https://example.com/data.csv"},{"file":{"file_system_id":"local","path":"#{p("/document.pdf")}"},"name":"document.pdf","type":"file"},{"name":"image.jpg","type":"attachment"}]} -->

      # My Notebook
      """

      {document, []} = Export.notebook_to_livemd(notebook, include_stamp: false)

      assert expected_document == document
    end

    test "stores quarantine file entry names if there are any :file file entries" do
      file = Livebook.FileSystem.File.new(Livebook.FileSystem.Local.new(), p("/document.pdf"))

      # All allowed

      notebook = %{
        Notebook.new()
        | name: "My Notebook",
          file_entries: [
            %{type: :file, name: "document.pdf", file: file}
          ]
      }

      {document, []} = Export.notebook_to_livemd(notebook)

      assert stamp_metadata(notebook, document) == %{quarantine_file_entry_names: []}

      # Subset allowed

      notebook = %{
        Notebook.new()
        | name: "My Notebook",
          file_entries: [
            %{type: :file, name: "document1.pdf", file: file},
            %{type: :file, name: "document2.pdf", file: file}
          ],
          quarantine_file_entry_names: MapSet.new(["document1.pdf"])
      }

      {document, []} = Export.notebook_to_livemd(notebook)

      assert stamp_metadata(notebook, document) == %{
               quarantine_file_entry_names: ["document1.pdf"]
             }
    end
  end

  defp stamp_metadata(notebook, source) do
    [_, json] = Regex.run(~r/<!-- livebook:(.*) -->\n$/, source)
    %{"offset" => offset, "stamp" => stamp} = Jason.decode!(json)

    hub = Livebook.Hubs.fetch_hub!(notebook.hub_id)
    source = binary_slice(source, 0, offset)
    {:ok, metadata} = Livebook.Hubs.verify_notebook_stamp(hub, source, stamp)
    metadata
  end

  defp spawn_widget_with_data(ref, data) do
    spawn(fn ->
      receive do
        {:connect, pid, %{ref: ^ref}} -> send(pid, {:connect_reply, data, %{ref: ref}})
      end
    end)
  end
end

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
                  Notebook.Cell.new(:input)
                  | type: :text,
                    name: "length",
                    value: "100"
                },
                %{
                  Notebook.Cell.new(:elixir)
                  | source: """
                    IO.gets("length: ")\
                    """
                },
                %{
                  Notebook.Cell.new(:input)
                  | type: :range,
                    name: "length",
                    value: "100",
                    props: %{min: 50, max: 150, step: 2}
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

    <!-- livebook:{"livebook_object":"cell_input","name":"length","type":"text","value":"100"} -->

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

    document = Export.notebook_to_markdown(notebook)

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

    document = Export.notebook_to_markdown(notebook)

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

    document = Export.notebook_to_markdown(notebook)

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

    document = Export.notebook_to_markdown(notebook)

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

    document = Export.notebook_to_markdown(notebook)

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

    document = Export.notebook_to_markdown(notebook)

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

    document = Export.notebook_to_markdown(notebook)

    assert expected_document == document
  end

  test "saves password as empty string" do
    notebook = %{
      Notebook.new()
      | name: "My Notebook",
        sections: [
          %{
            Notebook.Section.new()
            | name: "Section 1",
              cells: [
                %{
                  Notebook.Cell.new(:input)
                  | type: :password,
                    name: "pass",
                    value: "0123456789"
                }
              ]
          }
        ]
    }

    expected_document = """
    # My Notebook

    ## Section 1

    <!-- livebook:{"livebook_object":"cell_input","name":"pass","type":"password","value":""} -->
    """

    document = Export.notebook_to_markdown(notebook)

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

    document = Export.notebook_to_markdown(notebook)

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

    document = Export.notebook_to_markdown(notebook)

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
                        "hey",
                        {:vega_lite_static,
                         %{
                           "$schema" => "https://vega.github.io/schema/vega-lite/v5.json"
                         }}
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

      document = Export.notebook_to_markdown(notebook)

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
                      outputs: ["hey"]
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

      ```output
      hey
      ```
      """

      document = Export.notebook_to_markdown(notebook, include_outputs: true)

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
                      outputs: [{:text, "\e[34m:ok\e[0m"}, "hey"]
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

      ```output
      hey
      ```

      ```output
      :ok
      ```
      """

      document = Export.notebook_to_markdown(notebook, include_outputs: true)

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
                      outputs: [{:table_dynamic, self()}]
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

      document = Export.notebook_to_markdown(notebook, include_outputs: true)

      assert expected_document == document
    end

    test "includes vega_lite_static output" do
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
                      Vl.new(width: 500, height: 200)
                      |> Vl.data_from_series(in: [1, 2, 3, 4, 5], out: [1, 2, 3, 4, 5])
                      |> Vl.mark(:line)
                      |> Vl.encode_field(:x, "in", type: :quantitative)
                      |> Vl.encode_field(:y, "out", type: :quantitative)\
                      """,
                      outputs: [
                        {:vega_lite_static,
                         %{
                           "$schema" => "https://vega.github.io/schema/vega-lite/v5.json",
                           "data" => %{
                             "values" => [
                               %{"in" => 1, "out" => 1},
                               %{"in" => 2, "out" => 2},
                               %{"in" => 3, "out" => 3},
                               %{"in" => 4, "out" => 4},
                               %{"in" => 5, "out" => 5}
                             ]
                           },
                           "encoding" => %{
                             "x" => %{"field" => "in", "type" => "quantitative"},
                             "y" => %{"field" => "out", "type" => "quantitative"}
                           },
                           "height" => 200,
                           "mark" => "line",
                           "width" => 500
                         }}
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
      Vl.new(width: 500, height: 200)
      |> Vl.data_from_series(in: [1, 2, 3, 4, 5], out: [1, 2, 3, 4, 5])
      |> Vl.mark(:line)
      |> Vl.encode_field(:x, "in", type: :quantitative)
      |> Vl.encode_field(:y, "out", type: :quantitative)
      ```

      ```vega-lite
      {"$schema":"https://vega.github.io/schema/vega-lite/v5.json","data":{"values":[{"in":1,"out":1},{"in":2,"out":2},{"in":3,"out":3},{"in":4,"out":4},{"in":5,"out":5}]},"encoding":{"x":{"field":"in","type":"quantitative"},"y":{"field":"out","type":"quantitative"}},"height":200,"mark":"line","width":500}
      ```
      """

      document = Export.notebook_to_markdown(notebook, include_outputs: true)

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
                    outputs: ["hey"]
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

    ```output
    hey
    ```
    """

    document = Export.notebook_to_markdown(notebook)

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
                    outputs: ["hey"]
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

    document = Export.notebook_to_markdown(notebook, include_outputs: false)

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

    document = Export.notebook_to_markdown(notebook)

    assert expected_document == document
  end
end

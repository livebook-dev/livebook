defmodule Livebook.Session.DataSyncTest do
  use ExUnit.Case, async: true

  alias Livebook.Notebook
  alias Livebook.Notebook.Cell
  alias Livebook.Notebook.Section
  alias Livebook.Session.DataSync
  alias Livebook.Text.Delta

  @cid "__anonymous__"

  # Notebook attrs

  test "no changes returns empty list of operations" do
    notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "s1", cells: [%{Cell.new(:markdown) | id: "c1", source: "Hello"}]}
        ]
    }

    assert_ops_and_result(notebook, notebook, [])
  end

  test "notebook name changed" do
    before_notebook = %{Notebook.new() | name: "Old Name"}
    after_notebook = %{Notebook.new() | name: "New Name"}

    expected_ops = [{:set_notebook_attributes, @cid, %{name: "New Name"}}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "notebook leading_comments changed" do
    before_notebook = %{Notebook.new() | leading_comments: []}
    after_notebook = %{Notebook.new() | leading_comments: [["foo"]]}

    expected_ops = [{:set_notebook_attributes, @cid, %{leading_comments: [["foo"]]}}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  # Setup section

  test "language enabled" do
    before_notebook = Notebook.new()

    after_notebook = %{
      Notebook.add_extra_setup_cell(Notebook.new(), :python)
      | default_language: :python
    }

    expected_ops = [{:enable_language, @cid, :python}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "language disabled" do
    before_notebook = Notebook.add_extra_setup_cell(Notebook.new(), :python)
    after_notebook = Notebook.new()

    expected_ops = [{:disable_language, @cid, :python}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "setup cell source changed" do
    cell_id = Cell.main_setup_cell_id()

    before_notebook =
      Notebook.new()
      |> Notebook.put_setup_cells([%{Notebook.Cell.new(:code) | source: ""}])

    after_notebook =
      Notebook.new()
      |> Notebook.put_setup_cells([%{Notebook.Cell.new(:code) | source: "IO.inspect(:hello)"}])

    delta = Delta.diff("", "IO.inspect(:hello)")
    expected_ops = [{:apply_cell_delta, @cid, cell_id, :primary, delta, nil, 0}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  # Sections

  test "section inserted" do
    before_notebook = %{
      Notebook.new()
      | sections: [%{Section.new() | id: "s1", name: "Section 1"}]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "a-s1", name: "Section 1"},
          %{Section.new() | id: "a-s2"}
        ]
    }

    expected_ops = [{:insert_section_into, @cid, "s1", 0, "a-s2"}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "branching section inserted" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "s1", name: "Section 1"},
          %{Section.new() | id: "s2", name: "Section 2"}
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "a-s1", name: "Section 1"},
          %{Section.new() | id: "a-s2", name: "Section 2"},
          %{Section.new() | id: "a-s3", name: "Branch", parent_id: "a-s1"}
        ]
    }

    expected_ops = [
      {:insert_section_into, @cid, "s2", 0, "a-s3"},
      {:set_section_name, @cid, "a-s3", "Branch"},
      {:set_section_parent, @cid, "a-s3", "s1"}
    ]

    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "section deleted" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "s1"},
          %{Section.new() | id: "s2", name: "Extra"}
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [%{Section.new() | id: "a-s1"}]
    }

    expected_ops = [{:delete_section, @cid, "s2", false}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "section name changed" do
    before_notebook = %{
      Notebook.new()
      | sections: [%{Section.new() | id: "s1", name: "Old Name"}]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [%{Section.new() | id: "a-s1", name: "New Name"}]
    }

    expected_ops = [{:set_section_name, @cid, "s1", "New Name"}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "section parent set" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "s1"},
          %{Section.new() | id: "s2"}
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "a-s1"},
          %{Section.new() | id: "a-s2", parent_id: "a-s1"}
        ]
    }

    expected_ops = [{:set_section_parent, @cid, "s2", "s1"}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "section parent unset" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "s1"},
          %{Section.new() | id: "s2", parent_id: "s1"}
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "a-s1"},
          %{Section.new() | id: "a-s2"}
        ]
    }

    expected_ops = [{:unset_section_parent, @cid, "s2"}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "section with child sections deleted" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "s1", name: "Section 1"},
          %{Section.new() | id: "s2", name: "Section 2", parent_id: "s1"}
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "a-s2", name: "Section 2"}
        ]
    }

    expected_ops = [{:unset_section_parent, @cid, "s2"}, {:delete_section, @cid, "s1", false}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "section and child section both deleted" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "s1", name: "Section 1"},
          %{Section.new() | id: "s2", name: "Section 2"},
          %{Section.new() | id: "s3", name: "Section 3", parent_id: "s2"}
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "a-s1", name: "Section 1"}
        ]
    }

    expected_ops = [{:delete_section, @cid, "s3", false}, {:delete_section, @cid, "s2", false}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "leading section with cells deleted" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "s1",
              name: "Section 1",
              cells: [%{Cell.new(:markdown) | id: "c1"}]
          },
          %{Section.new() | id: "s2", name: "Section 2"}
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "a-s2", name: "Section 2"}
        ]
    }

    expected_ops = [{:delete_cell, @cid, "c1"}, {:delete_section, @cid, "s1", false}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  # Cells - insert / delete

  test "markdown cell inserted" do
    before_notebook = %{Notebook.new() | sections: [%{Section.new() | id: "s1"}]}

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "a-s1",
              cells: [%{Cell.new(:markdown) | id: "a-c1", source: "New cell"}]
          }
        ]
    }

    expected_ops = [{:insert_cell, @cid, "s1", 0, :markdown, "a-c1", %{source: "New cell"}}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "code cell inserted" do
    before_notebook = %{Notebook.new() | sections: [%{Section.new() | id: "s1"}]}

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "a-s1", cells: [%{Cell.new(:code) | id: "a-c1", source: "x = 1"}]}
        ]
    }

    expected_ops = [{:insert_cell, @cid, "s1", 0, :code, "a-c1", %{source: "x = 1"}}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "cell deleted" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "s1", cells: [%{Cell.new(:markdown) | id: "c1", source: "Hello"}]}
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [%{Section.new() | id: "a-s1"}]
    }

    expected_ops = [{:delete_cell, @cid, "c1"}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  # Cell updates

  test "markdown cell source updated" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "s1", cells: [%{Cell.new(:markdown) | id: "c1", source: "Hello"}]}
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "a-s1",
              cells: [%{Cell.new(:markdown) | id: "a-c1", source: "Hello world"}]
          }
        ]
    }

    delta = Delta.diff("Hello", "Hello world")
    expected_ops = [{:apply_cell_delta, @cid, "c1", :primary, delta, nil, 0}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "code cell source updated" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "s1", cells: [%{Cell.new(:code) | id: "c1", source: "x = 1"}]}
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "a-s1", cells: [%{Cell.new(:code) | id: "a-c1", source: "x = 2"}]}
        ]
    }

    delta = Delta.diff("x = 1", "x = 2")
    expected_ops = [{:apply_cell_delta, @cid, "c1", :primary, delta, nil, 0}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "code cell attrs updated" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "s1", cells: [%{Cell.new(:code) | id: "c1", source: "x = 1"}]}
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "a-s1",
              cells: [
                %{
                  Cell.new(:code)
                  | id: "a-c1",
                    source: "x = 1",
                    reevaluate_automatically: true,
                    continue_on_error: true,
                    output_size: :wide
                }
              ]
          }
        ]
    }

    expected_ops = [
      {:set_cell_attributes, @cid, "c1",
       %{reevaluate_automatically: true, continue_on_error: true, output_size: :wide}}
    ]

    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "code cell source and attrs both updated" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{Section.new() | id: "s1", cells: [%{Cell.new(:code) | id: "c1", source: "x = 1"}]}
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "a-s1",
              cells: [
                %{Cell.new(:code) | id: "a-c1", source: "x = 2", reevaluate_automatically: true}
              ]
          }
        ]
    }

    delta = Delta.diff("x = 1", "x = 2")

    expected_ops = [
      {:apply_cell_delta, @cid, "c1", :primary, delta, nil, 0},
      {:set_cell_attributes, @cid, "c1", %{reevaluate_automatically: true}}
    ]

    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "smart cell output size updated" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "s1",
              cells: [%{Cell.new(:smart) | id: "c1", source: "x = 1", attrs: %{}}]
          }
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "a-s1",
              cells: [
                %{
                  Cell.new(:smart)
                  | id: "a-c1",
                    source: "x = 2",
                    attrs: %{foo: :bar},
                    output_size: :full
                }
              ]
          }
        ]
    }

    expected_ops = [{:set_cell_attributes, @cid, "c1", %{output_size: :full}}]
    data = Livebook.Session.Data.new(notebook: before_notebook)
    assert DataSync.sync(data, after_notebook, @cid) == expected_ops
  end

  # Cell moves

  test "cell moved up within section" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "s1",
              cells: [
                %{Cell.new(:code) | id: "c1", source: "A"},
                %{Cell.new(:code) | id: "c2", source: "B"},
                %{Cell.new(:code) | id: "c3", source: "C"}
              ]
          }
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "a-s1",
              cells: [
                %{Cell.new(:code) | id: "a-c3", source: "C"},
                %{Cell.new(:code) | id: "a-c1", source: "A"},
                %{Cell.new(:code) | id: "a-c2", source: "B"}
              ]
          }
        ]
    }

    expected_ops = [{:move_cell, @cid, "c3", "s1", 0}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "cell moved down within section" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "s1",
              cells: [
                %{Cell.new(:code) | id: "c1", source: "A"},
                %{Cell.new(:code) | id: "c2", source: "B"},
                %{Cell.new(:code) | id: "c3", source: "C"}
              ]
          }
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "a-s1",
              cells: [
                %{Cell.new(:code) | id: "a-c2", source: "B"},
                %{Cell.new(:code) | id: "a-c3", source: "C"},
                %{Cell.new(:code) | id: "a-c1", source: "A"}
              ]
          }
        ]
    }

    expected_ops = [{:move_cell, @cid, "c1", "s1", 3}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "cell moved from earlier section to later section" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "s1",
              cells: [
                %{Cell.new(:code) | id: "c1", source: "A"},
                %{Cell.new(:code) | id: "c2", source: "B"}
              ]
          },
          %{
            Section.new()
            | id: "s2",
              cells: [%{Cell.new(:code) | id: "c3", source: "C"}]
          }
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "a-s1",
              cells: [%{Cell.new(:code) | id: "a-c2", source: "B"}]
          },
          %{
            Section.new()
            | id: "a-s2",
              cells: [
                %{Cell.new(:code) | id: "a-c1", source: "A"},
                %{Cell.new(:code) | id: "a-c3", source: "C"}
              ]
          }
        ]
    }

    expected_ops = [{:move_cell, @cid, "c1", "s2", 0}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "cell moved from later section to earlier section" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "s1",
              cells: [%{Cell.new(:code) | id: "c2", source: "B"}]
          },
          %{
            Section.new()
            | id: "s2",
              cells: [
                %{Cell.new(:code) | id: "c1", source: "A"},
                %{Cell.new(:code) | id: "c3", source: "C"}
              ]
          }
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "a-s1",
              cells: [
                %{Cell.new(:code) | id: "a-c1", source: "A"},
                %{Cell.new(:code) | id: "a-c2", source: "B"}
              ]
          },
          %{
            Section.new()
            | id: "a-s2",
              cells: [%{Cell.new(:code) | id: "a-c3", source: "C"}]
          }
        ]
    }

    expected_ops = [{:move_cell, @cid, "c1", "s1", 0}]
    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "multiple cells with same source moved down together" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "s1",
              cells: [
                %{Cell.new(:code) | id: "c1", source: "A"},
                %{Cell.new(:code) | id: "c2", source: "A"},
                %{Cell.new(:code) | id: "c3", source: "A"},
                %{Cell.new(:code) | id: "c4", source: "B"},
                %{Cell.new(:code) | id: "c5", source: "C"}
              ]
          }
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "a-s1",
              cells: [
                %{Cell.new(:code) | id: "a-c1", source: "A"},
                %{Cell.new(:code) | id: "a-c4", source: "B"},
                %{Cell.new(:code) | id: "a-c5", source: "C"},
                %{Cell.new(:code) | id: "a-c2", source: "A"},
                %{Cell.new(:code) | id: "a-c3", source: "A"}
              ]
          }
        ]
    }

    expected_ops = [
      {:move_cell, @cid, "c2", "s1", 5},
      {:move_cell, @cid, "c3", "s1", 5}
    ]

    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "multiple cells with same source moved up together" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "s1",
              cells: [
                %{Cell.new(:code) | id: "c1", source: "A"},
                %{Cell.new(:code) | id: "c2", source: "B"},
                %{Cell.new(:code) | id: "c3", source: "C"},
                %{Cell.new(:code) | id: "c4", source: "C"},
                %{Cell.new(:code) | id: "c5", source: "C"}
              ]
          }
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "a-s1",
              cells: [
                %{Cell.new(:code) | id: "a-c1", source: "C"},
                %{Cell.new(:code) | id: "a-c4", source: "C"},
                %{Cell.new(:code) | id: "a-c5", source: "A"},
                %{Cell.new(:code) | id: "a-c2", source: "B"},
                %{Cell.new(:code) | id: "a-c3", source: "C"}
              ]
          }
        ]
    }

    expected_ops = [
      {:move_cell, @cid, "c4", "s1", 0},
      {:move_cell, @cid, "c5", "s1", 1}
    ]

    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "multiple cells moved down with different relative order" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "s1",
              cells: [
                %{Cell.new(:code) | id: "c1", source: "A"},
                %{Cell.new(:code) | id: "c2", source: "B"},
                %{Cell.new(:code) | id: "c3", source: "C"},
                %{Cell.new(:code) | id: "c4", source: "D"},
                %{Cell.new(:code) | id: "c5", source: "E"},
                %{Cell.new(:code) | id: "c6", source: "F"}
              ]
          }
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "a-s1",
              cells: [
                %{Cell.new(:code) | id: "a-c4", source: "D"},
                %{Cell.new(:code) | id: "a-c5", source: "E"},
                %{Cell.new(:code) | id: "a-c6", source: "F"},
                %{Cell.new(:code) | id: "a-c2", source: "B"},
                %{Cell.new(:code) | id: "a-c1", source: "A"},
                %{Cell.new(:code) | id: "a-c3", source: "C"}
              ]
          }
        ]
    }

    expected_ops = [
      {:move_cell, @cid, "c2", "s1", 6},
      {:move_cell, @cid, "c1", "s1", 6},
      {:move_cell, @cid, "c3", "s1", 6}
    ]

    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "multiple cells moved up with different relative order" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "s1",
              cells: [
                %{Cell.new(:code) | id: "c1", source: "A"},
                %{Cell.new(:code) | id: "c2", source: "B"},
                %{Cell.new(:code) | id: "c3", source: "C"},
                %{Cell.new(:code) | id: "c4", source: "D"},
                %{Cell.new(:code) | id: "c5", source: "E"},
                %{Cell.new(:code) | id: "c6", source: "F"}
              ]
          }
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "a-s1",
              cells: [
                %{Cell.new(:code) | id: "a-c6", source: "F"},
                %{Cell.new(:code) | id: "a-c4", source: "D"},
                %{Cell.new(:code) | id: "a-c5", source: "E"},
                %{Cell.new(:code) | id: "a-c1", source: "A"},
                %{Cell.new(:code) | id: "a-c2", source: "B"},
                %{Cell.new(:code) | id: "a-c3", source: "C"}
              ]
          }
        ]
    }

    expected_ops = [
      {:move_cell, @cid, "c6", "s1", 0},
      {:move_cell, @cid, "c4", "s1", 1},
      {:move_cell, @cid, "c5", "s1", 2}
    ]

    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  test "multiple cells moved up and down with different relative order" do
    before_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "s1",
              cells: [
                %{Cell.new(:code) | id: "c1", source: "A"},
                %{Cell.new(:code) | id: "c2", source: "B"},
                %{Cell.new(:code) | id: "c3", source: "C"},
                %{Cell.new(:code) | id: "c4", source: "D"},
                %{Cell.new(:code) | id: "c5", source: "E"},
                %{Cell.new(:code) | id: "c6", source: "F"},
                %{Cell.new(:code) | id: "c7", source: "G"}
              ]
          }
        ]
    }

    after_notebook = %{
      Notebook.new()
      | sections: [
          %{
            Section.new()
            | id: "a-s1",
              cells: [
                %{Cell.new(:code) | id: "a-c7", source: "G"},
                %{Cell.new(:code) | id: "a-c6", source: "F"},
                %{Cell.new(:code) | id: "a-c3", source: "C"},
                %{Cell.new(:code) | id: "a-c4", source: "D"},
                %{Cell.new(:code) | id: "a-c5", source: "E"},
                %{Cell.new(:code) | id: "a-c2", source: "B"},
                %{Cell.new(:code) | id: "a-c1", source: "A"}
              ]
          }
        ]
    }

    expected_ops = [
      {:move_cell, @cid, "c7", "s1", 2},
      {:move_cell, @cid, "c6", "s1", 3},
      {:move_cell, @cid, "c2", "s1", 7},
      {:move_cell, @cid, "c1", "s1", 7}
    ]

    assert_ops_and_result(before_notebook, after_notebook, expected_ops)
  end

  defp assert_ops_and_result(before_notebook, after_notebook, expected_ops) do
    data = Livebook.Session.Data.new(notebook: before_notebook)

    sync_ops = DataSync.sync(data, after_notebook, @cid)
    assert sync_ops == expected_ops

    # Check that all operations can be applied successfully and that
    # the livemd matches afterwards.

    data =
      Enum.reduce(sync_ops, data, fn operation, data ->
        {:ok, data, _actions} = Livebook.Session.Data.apply_operation(data, operation)
        data
      end)

    {final_livemd, []} = Livebook.LiveMarkdown.notebook_to_livemd(data.notebook)
    {expected_livemd, []} = Livebook.LiveMarkdown.notebook_to_livemd(after_notebook)
    assert final_livemd == expected_livemd
  end
end

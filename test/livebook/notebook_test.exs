defmodule Livebook.NotebookTest do
  use ExUnit.Case, async: true

  alias Livebook.Notebook
  alias Livebook.Notebook.{Section, Cell}

  describe "fetch_cell_sibling/3" do
    test "returns error given invalid cell id" do
      notebook = Notebook.new()

      assert :error == Notebook.fetch_cell_sibling(notebook, "1", 0)
    end

    test "returns sibling cell if there is one at the given offset" do
      cell1 = Cell.new(:markdown)
      cell2 = Cell.new(:markdown)
      cell3 = Cell.new(:markdown)
      cell4 = Cell.new(:markdown)

      notebook = %{
        Notebook.new()
        | sections: [
            %{Section.new() | cells: [cell1, cell2, cell3, cell4]}
          ]
      }

      assert {:ok, cell1} == Notebook.fetch_cell_sibling(notebook, cell2.id, -1)
      assert {:ok, cell3} == Notebook.fetch_cell_sibling(notebook, cell2.id, 1)
      assert {:ok, cell4} == Notebook.fetch_cell_sibling(notebook, cell2.id, 2)
    end

    test "returns error if the offset is out of range" do
      cell1 = Cell.new(:markdown)
      cell2 = Cell.new(:markdown)

      notebook = %{
        Notebook.new()
        | sections: [
            %{Section.new() | cells: [cell1, cell2]}
          ]
      }

      assert :error == Notebook.fetch_cell_sibling(notebook, cell2.id, -2)
      assert :error == Notebook.fetch_cell_sibling(notebook, cell2.id, 1)
      assert :error == Notebook.fetch_cell_sibling(notebook, cell2.id, 2)
    end
  end

  describe "move_cell/3" do
    test "preserves empty sections" do
      cell1 = Cell.new(:markdown)

      notebook = %{
        Notebook.new()
        | sections: [
            %{Section.new() | cells: [cell1]},
            %{Section.new() | cells: []}
          ]
      }

      new_notebook = Notebook.move_cell(notebook, cell1.id, 1)

      assert %{sections: [%{cells: []}, %{cells: [^cell1]}]} = new_notebook
    end
  end

  describe "cell_dependency_graph/1" do
    test "computes a linear graph for regular sections" do
      notebook = %{
        Notebook.new()
        | sections: [
            %{
              Section.new()
              | id: "s1",
                cells: [
                  %{Cell.new(:markdown) | id: "c1"},
                  %{Cell.new(:elixir) | id: "c2"}
                ]
            },
            %{
              Section.new()
              | id: "s2",
                cells: [
                  %{Cell.new(:markdown) | id: "c3"},
                  %{Cell.new(:elixir) | id: "c4"}
                ]
            }
          ]
      }

      assert Notebook.cell_dependency_graph(notebook) == %{
               "c4" => "c3",
               "c3" => "c2",
               "c2" => "c1",
               "c1" => nil
             }
    end

    test "ignores empty sections" do
      notebook = %{
        Notebook.new()
        | sections: [
            %{Section.new() | id: "s1", cells: [%{Cell.new(:elixir) | id: "c1"}]},
            %{Section.new() | id: "s2", cells: []},
            %{Section.new() | id: "s3", cells: [%{Cell.new(:elixir) | id: "c2"}]}
          ]
      }

      assert Notebook.cell_dependency_graph(notebook) == %{
               "c2" => "c1",
               "c1" => nil
             }
    end

    test "computes a non-linear graph if there are branching sections" do
      notebook = %{
        Notebook.new()
        | sections: [
            %{
              Section.new()
              | id: "s1",
                cells: [
                  %{Cell.new(:elixir) | id: "c1"},
                  %{Cell.new(:elixir) | id: "c2"}
                ]
            },
            %{
              Section.new()
              | id: "s2",
                parent_id: "s1",
                cells: [
                  %{Cell.new(:elixir) | id: "c3"},
                  %{Cell.new(:elixir) | id: "c4"}
                ]
            },
            %{
              Section.new()
              | id: "s3",
                cells: [
                  %{Cell.new(:elixir) | id: "c5"},
                  %{Cell.new(:elixir) | id: "c6"}
                ]
            }
          ]
      }

      assert Notebook.cell_dependency_graph(notebook) == %{
               "c6" => "c5",
               "c5" => "c2",
               "c4" => "c3",
               "c3" => "c2",
               "c2" => "c1",
               "c1" => nil
             }
    end

    test "handles branching sections pointing to empty sections" do
      notebook = %{
        Notebook.new()
        | sections: [
            %{
              Section.new()
              | id: "s1",
                cells: [
                  %{Cell.new(:elixir) | id: "c1"}
                ]
            },
            %{
              Section.new()
              | id: "s2",
                cells: []
            },
            %{
              Section.new()
              | id: "s3",
                parent_id: "s2",
                cells: [
                  %{Cell.new(:elixir) | id: "c2"}
                ]
            }
          ]
      }

      assert Notebook.cell_dependency_graph(notebook) == %{
               "c2" => "c1",
               "c1" => nil
             }
    end

    test "handles branching sections placed further in the notebook" do
      notebook = %{
        Notebook.new()
        | sections: [
            %{
              Section.new()
              | id: "s1",
                cells: [
                  %{Cell.new(:elixir) | id: "c1"}
                ]
            },
            %{
              Section.new()
              | id: "s2",
                cells: [
                  %{Cell.new(:elixir) | id: "c2"}
                ]
            },
            %{
              Section.new()
              | id: "s3",
                parent_id: "s1",
                cells: [
                  %{Cell.new(:elixir) | id: "c3"}
                ]
            },
            %{
              Section.new()
              | id: "s4",
                cells: [
                  %{Cell.new(:elixir) | id: "c4"}
                ]
            }
          ]
      }

      assert Notebook.cell_dependency_graph(notebook) == %{
               "c4" => "c2",
               "c3" => "c1",
               "c2" => "c1",
               "c1" => nil
             }
    end

    test "given :cell_filter option, includes only the matching cells" do
      notebook = %{
        Notebook.new()
        | sections: [
            %{
              Section.new()
              | id: "s1",
                cells: [
                  %{Cell.new(:elixir) | id: "c1"},
                  %{Cell.new(:markdown) | id: "c2"},
                  %{Cell.new(:elixir) | id: "c3"}
                ]
            }
          ]
      }

      assert Notebook.cell_dependency_graph(notebook, cell_filter: &is_struct(&1, Cell.Elixir)) ==
               %{
                 "c3" => "c1",
                 "c1" => nil
               }
    end
  end

  describe "find_asset_info/2" do
    test "returns asset info matching the given type if found" do
      assets_info = %{archive: "/path/to/archive.tar.gz", hash: "abcd", js_path: "main.js"}
      js_info = %{assets: assets_info}
      output = {:js, js_info}

      notebook = %{
        Notebook.new()
        | sections: [%{Section.new() | cells: [%{Cell.new(:elixir) | outputs: [output]}]}]
      }

      assert ^assets_info = Notebook.find_asset_info(notebook, "abcd")
    end

    test "returns nil if no matching info is found" do
      notebook = Notebook.new()
      assert Notebook.find_asset_info(notebook, "abcd") == nil
    end
  end
end

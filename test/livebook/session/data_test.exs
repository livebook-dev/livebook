defmodule Livebook.Session.DataTest do
  use ExUnit.Case, async: true

  import Livebook.TestHelpers

  alias Livebook.Session.Data
  alias Livebook.{Delta, Notebook}
  alias Livebook.Users.User

  @eval_resp {:ok, [1, 2, 3]}
  @smart_cell_definitions [%{kind: "text", name: "Text", requirement_presets: []}]
  @cid "__anonymous__"

  defp eval_meta(opts \\ []) do
    uses = opts[:uses] || []
    defines = opts[:defines] || %{}
    errored = Keyword.get(opts, :errored, false)
    interrupted = Keyword.get(opts, :interrupted, false)

    %{
      errored: errored,
      interrupted: interrupted,
      evaluation_time_ms: 10,
      identifiers_used: uses,
      identifiers_defined: defines,
      code_markers: []
    }
  end

  describe "new/1" do
    test "called with no arguments defaults to a blank notebook" do
      assert %{notebook: %{sections: []}, cell_infos: cell_infos, section_infos: section_infos} =
               Data.new()

      assert map_size(cell_infos) == 1
      assert map_size(section_infos) == 1
    end

    test "called with a notebook, sets default cell and section infos" do
      notebook = %{
        Notebook.new()
        | sections: [
            %{
              Notebook.Section.new()
              | id: "s1",
                cells: [%{Notebook.Cell.new(:code) | id: "c1"}]
            }
          ]
      }

      assert %{cell_infos: %{"c1" => %{}}, section_infos: %{"s1" => %{}}} =
               Data.new(notebook: notebook)
    end

    test "called with a notebook, computes cell snapshots" do
      notebook = %{
        Notebook.new()
        | sections: [
            %{
              Notebook.Section.new()
              | id: "s1",
                cells: [%{Notebook.Cell.new(:code) | id: "c1"}]
            }
          ]
      }

      assert %{cell_infos: %{"c1" => %{eval: %{snapshot: snapshot}}}} =
               Data.new(notebook: notebook)

      assert snapshot != nil
    end

    test "clears all outputs when in app mode" do
      notebook = %{
        Notebook.new()
        | sections: [
            %{
              Notebook.Section.new()
              | id: "s1",
                cells: [
                  %{Notebook.Cell.new(:code) | id: "c1", outputs: [{0, {:stdout, "Hello!"}}]}
                ]
            }
          ]
      }

      assert %{notebook: %{sections: [%{cells: [%{outputs: [{0, {:stdout, "Hello!"}}]}]}]}} =
               Data.new(notebook: notebook)

      assert %{notebook: %{sections: [%{cells: [%{outputs: []}]}]}} =
               Data.new(notebook: notebook, mode: :app)
    end
  end

  describe "apply_operation/2 given :insert_section" do
    test "adds new section to notebook and section info" do
      data = Data.new()

      operation = {:insert_section, @cid, 0, "s1"}

      assert {:ok,
              %{
                notebook: %{
                  sections: [%{id: "s1"}]
                },
                section_infos: %{"s1" => _}
              }, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :set_notebook_attributes" do
    test "returns an error given an unknown attribute key" do
      data = Data.new()

      attrs = %{unknown: :value}
      operation = {:set_notebook_attributes, @cid, attrs}

      assert :error = Data.apply_operation(data, operation)
    end

    test "updates notebook with the given attributes" do
      data = Data.new()

      attrs = %{persist_outputs: true}
      operation = {:set_notebook_attributes, @cid, attrs}

      assert {:ok,
              %{
                notebook: %{persist_outputs: true}
              }, _} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :insert_section_into" do
    test "returns an error given invalid section id" do
      data = Data.new()
      operation = {:insert_section_into, @cid, "nonexistent", 0, "s1"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "adds new section below the given one and section info" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"}
        ])

      operation = {:insert_section_into, @cid, "s1", 0, "s2"}

      assert {:ok,
              %{
                notebook: %{
                  sections: [%{id: "s1"}, %{id: "s2"}]
                },
                section_infos: %{"s2" => _}
              }, []} = Data.apply_operation(data, operation)
    end

    test "moves cells below the given index to the newly inserted section" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}}
        ])

      operation = {:insert_section_into, @cid, "s1", 1, "s2"}

      assert {:ok,
              %{
                notebook: %{
                  sections: [%{cells: [%{id: "c1"}]}, %{cells: [%{id: "c2"}]}]
                }
              }, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :set_section_parent" do
    test "returns an error given invalid section id" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"}
        ])

      operation = {:set_section_parent, @cid, "nonexistent", "s1"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given invalid parent section id" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"}
        ])

      operation = {:set_section_parent, @cid, "s1", "nonexistent"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error if the parent section is below the given section" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_section, @cid, 1, "s2"}
        ])

      operation = {:set_section_parent, @cid, "s1", "s2"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error if the parent section is a branch section" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_section, @cid, 1, "s2"},
          {:insert_section, @cid, 2, "s3"},
          {:set_section_parent, @cid, "s2", "s1"}
        ])

      operation = {:set_section_parent, @cid, "s3", "s2"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error if there are sections branching out from the given section" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_section, @cid, 1, "s2"},
          {:insert_section, @cid, 2, "s3"},
          {:set_section_parent, @cid, "s3", "s2"}
        ])

      operation = {:set_section_parent, @cid, "s2", "s1"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "sets parent id on section" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_section, @cid, 1, "s2"}
        ])

      operation = {:set_section_parent, @cid, "s2", "s1"}

      assert {:ok,
              %{
                notebook: %{
                  sections: [%{id: "s1", parent_id: nil}, %{id: "s2", parent_id: "s1"}]
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "marks cells in the given section (and dependent cells) as stale" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c3", %{}},
          {:insert_cell, @cid, "s3", 1, :code, "c4", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3", "c4"],
            uses: %{"c3" => ["c2"]}
          )
        ])

      operation = {:set_section_parent, @cid, "s2", "s1"}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :evaluated}},
                  "c2" => %{eval: %{validity: :stale}},
                  "c3" => %{eval: %{validity: :stale}},
                  "c4" => %{eval: %{validity: :evaluated}}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "if a cell is evaluating in the given section, clears all sections evaluation and queues" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c3", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"]),
          {:queue_cells_evaluation, @cid, ["c2", "c3"]}
        ])

      operation = {:set_section_parent, @cid, "s2", "s1"}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :aborted, status: :ready}},
                  "c2" => %{eval: %{validity: :aborted, status: :ready}},
                  "c3" => %{eval: %{validity: :fresh, status: :ready}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil},
                  "s2" => %{evaluating_cell_id: nil},
                  "s3" => %{evaluating_cell_id: nil}
                }
              } = new_data,
              [{:stop_evaluation, %{id: "s2", parent_id: nil}}]} =
               Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s2"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s3"].evaluation_queue == MapSet.new([])
    end
  end

  describe "apply_operation/2 given :unset_section_parent" do
    test "returns an error given invalid section id" do
      data = Data.new()
      operation = {:unset_section_parent, @cid, "nonexistent"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given section with no parent" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"}
        ])

      operation = {:unset_section_parent, @cid, "s1"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "sets parent id on section to nil" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_section, @cid, 1, "s2"},
          {:set_section_parent, @cid, "s2", "s1"}
        ])

      operation = {:unset_section_parent, @cid, "s2"}

      assert {:ok,
              %{
                notebook: %{
                  sections: [%{id: "s1", parent_id: nil}, %{id: "s2", parent_id: nil}]
                }
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "returns stop evaluation action" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_section, @cid, 1, "s2"},
          {:set_section_parent, @cid, "s2", "s1"}
        ])

      operation = {:unset_section_parent, @cid, "s2"}

      assert {:ok, %{}, [{:stop_evaluation, %{id: "s2", parent_id: "s1"}}]} =
               Data.apply_operation(data, operation)
    end

    test "marks cells in the given section (and dependent cells) as stale" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c3", %{}},
          {:set_section_parent, @cid, "s2", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3"])
        ])

      operation = {:unset_section_parent, @cid, "s2"}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :evaluated}},
                  "c2" => %{eval: %{validity: :stale}},
                  "c3" => %{eval: %{validity: :evaluated}}
                }
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "if a cell is evaluating in this section, marks evaluated and evaluating cells as aborted" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c3", %{}},
          {:set_section_parent, @cid, "s2", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"]),
          {:queue_cells_evaluation, @cid, ["c2", "c3"]}
        ])

      operation = {:unset_section_parent, @cid, "s2"}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :evaluated}},
                  "c2" => %{eval: %{validity: :aborted, status: :ready}},
                  "c3" => %{eval: %{validity: :fresh, status: :evaluating}}
                }
              }, _actions} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :insert_cell" do
    test "returns an error given invalid section id" do
      data = Data.new()
      operation = {:insert_cell, @cid, "nonexistent", 0, :code, "c1", %{}}
      assert :error = Data.apply_operation(data, operation)
    end

    test "adds a new cell to notebook and cell infos" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"}
        ])

      operation = {:insert_cell, @cid, "s1", 0, :code, "c1", %{}}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{cells: [%Notebook.Cell.Code{id: "c1"}]}
                  ]
                },
                cell_infos: %{"c1" => _}
              }, []} = Data.apply_operation(data, operation)
    end

    test "initializes client-revision map" do
      client_id = "client1"

      data =
        data_after_operations!([
          {:client_join, client_id, User.new()},
          {:insert_section, @cid, 0, "s1"}
        ])

      operation = {:insert_cell, @cid, "s1", 0, :code, "c1", %{}}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{sources: %{primary: %{revision_by_client_id: %{^client_id => 0}}}}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "given a smart cell, keeps it dead if there is no corresponding definition" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:set_runtime, @cid, connected_noop_runtime()}
        ])

      operation = {:insert_cell, @cid, "s1", 0, :smart, "c1", %{kind: "text"}}

      assert {:ok, %{cell_infos: %{"c1" => %{status: :dead}}}, []} =
               Data.apply_operation(data, operation)
    end

    test "given a smart cell, marks it as starting if there is a corresponding definition and a runtime" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          {:set_smart_cell_definitions, @cid, @smart_cell_definitions}
        ])

      operation = {:insert_cell, @cid, "s1", 0, :smart, "c1", %{kind: "text"}}

      assert {:ok, %{cell_infos: %{"c1" => %{status: :starting}}},
              [{:start_smart_cell, %{id: "c1"}, %{id: "s1"}}]} =
               Data.apply_operation(data, operation)
    end

    test "inserting code cell before smart cell does not change its parents" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :smart, "c2", %{kind: "text"}},
          {:set_runtime, @cid, connected_noop_runtime()},
          {:set_smart_cell_definitions, @cid, @smart_cell_definitions},
          {:smart_cell_started, @cid, "c2", Delta.new(), nil, %{}, nil}
        ])

      operation = {:insert_cell, @cid, "s1", 0, :code, "c3", %{}}

      assert {:ok, %{}, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :delete_section" do
    test "returns an error given invalid section id" do
      data = Data.new()
      operation = {:delete_section, @cid, "nonexistent", true}
      assert :error = Data.apply_operation(data, operation)
    end

    test "removes the section from notebook and section info" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"}
        ])

      operation = {:delete_section, @cid, "s1", true}

      assert {:ok,
              %{
                notebook: %{
                  sections: []
                },
                section_infos: section_infos
              }, []} = Data.apply_operation(data, operation)

      refute Map.has_key?(section_infos, "s1")
    end

    test "returns error when cell deletion is disabled for the first cell" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}}
        ])

      operation = {:delete_section, @cid, "s1", false}
      assert :error = Data.apply_operation(data, operation)
    end

    test "keeps cells when cell deletion is disabled" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}}
        ])

      operation = {:delete_section, @cid, "s2", false}

      assert {:ok,
              %{
                notebook: %{
                  sections: [%{id: "s1", cells: [%{id: "c1"}, %{id: "c2"}]}]
                },
                bin_entries: []
              }, []} = Data.apply_operation(data, operation)
    end

    test "deletes cells when cell deletion is enabled" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2"])
        ])

      operation = {:delete_section, @cid, "s2", true}

      assert {:ok,
              %{
                notebook: %{
                  sections: [%{id: "s1", cells: [%{id: "c1"}]}]
                },
                bin_entries: [
                  %{
                    cell: %{id: "c2"},
                    section_id: "s2",
                    section_name: "Section",
                    index: 0,
                    deleted_at: _
                  }
                ]
              },
              [{:forget_evaluation, %{id: "c2"}, %{id: "s2"}}]} =
               Data.apply_operation(data, operation)
    end

    test "marks dependent evaluated cells as stale when cells get deleted" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_cell, @cid, "s2", 1, :code, "c3", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3"], uses: %{"c2" => ["c1"]})
        ])

      operation = {:delete_section, @cid, "s1", true}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{eval: %{validity: :stale}},
                  "c3" => %{eval: %{validity: :evaluated}}
                }
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "when deleting a branching section, marks cells in the given section (and dependent cells) as stale" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c3", %{}},
          {:insert_cell, @cid, "s3", 1, :code, "c4", %{}},
          {:set_section_parent, @cid, "s2", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3", "c4"],
            uses: %{"c3" => ["c2"]}
          )
        ])

      operation = {:delete_section, @cid, "s2", false}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :evaluated}},
                  "c2" => %{eval: %{validity: :stale}},
                  "c3" => %{eval: %{validity: :stale}},
                  "c4" => %{eval: %{validity: :evaluated}}
                }
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "returns an error if the section has branching sections" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_section, @cid, 1, "s2"},
          {:set_section_parent, @cid, "s2", "s1"}
        ])

      operation = {:delete_section, @cid, "s1", false}

      assert :error = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :delete_cell" do
    test "returns an error given invalid cell id" do
      data = Data.new()
      operation = {:delete_cell, @cid, "nonexistent"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "if the cell is evaluating, clears section evaluation" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2"]}
        ])

      operation = {:delete_cell, @cid, "c1"}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{eval: %{status: :ready}}},
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil}
                }
              } = new_data, _actions} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
    end

    test "removes the cell from notebook and section info, adds to deleted cells" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}}
        ])

      operation = {:delete_cell, @cid, "c1"}

      assert {:ok,
              %{
                notebook: %{
                  sections: [%{cells: []}]
                },
                cell_infos: cell_infos,
                bin_entries: [
                  %{
                    cell: %{id: "c1"},
                    section_id: "s1",
                    section_name: "Section",
                    index: 0,
                    deleted_at: _
                  }
                ]
              }, _actions} = Data.apply_operation(data, operation)

      refute Map.has_key?(cell_infos, "c1")
    end

    test "unqueues the cell if it's queued for evaluation" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2"]}
        ])

      operation = {:delete_cell, @cid, "c2"}

      assert {:ok, new_data, _actions} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
    end

    test "marks evaluated dependent cells as stale" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3"],
            uses: %{"c2" => ["c1"]}
          )
        ])

      operation = {:delete_cell, @cid, "c1"}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{eval: %{validity: :stale}},
                  "c3" => %{eval: %{validity: :evaluated}}
                }
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "marks dependent automatically reevaluating cells for evaluation" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_cell_attributes, @cid, "c2", %{reevaluate_automatically: true}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2"],
            uses: %{"c2" => ["c1"]}
          )
        ])

      operation = {:delete_cell, @cid, "c1"}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{eval: %{status: :evaluating}}}
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "deleting a markdown cell does not change cell validity" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :markdown, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c2"])
        ])

      operation = {:delete_cell, @cid, "c1"}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{eval: %{validity: :evaluated}}}
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "returns forget evaluation action" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"])
        ])

      operation = {:delete_cell, @cid, "c1"}

      assert {:ok, _data, [{:forget_evaluation, %{id: "c1"}, %{id: "s1"}}]} =
               Data.apply_operation(data, operation)
    end

    test "given an alive smart cell returns a stop action" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          {:set_smart_cell_definitions, @cid, @smart_cell_definitions},
          {:insert_cell, @cid, "s1", 0, :smart, "c1", %{kind: "text"}},
          {:smart_cell_started, @cid, "c1", Delta.new(), nil, %{}, nil}
        ])

      operation = {:delete_cell, @cid, "c1"}

      assert {:ok, _data, [{:stop_smart_cell, %{id: "c1"}}]} =
               Data.apply_operation(data, operation)
    end

    test "given evaluated code cell, triggers child smart cell parents update" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :smart, "c2", %{kind: "text"}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:set_smart_cell_definitions, @cid, @smart_cell_definitions},
          {:smart_cell_started, @cid, "c2", Delta.new(), nil, %{}, nil},
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta()}
        ])

      operation = {:delete_cell, @cid, "c1"}

      assert {:ok, %{},
              [
                {:forget_evaluation, _, _},
                {:set_smart_cell_parents, %{id: "c2"}, %{id: "s1"},
                 [{%{id: "setup"}, %{id: "setup-section"}}]}
              ]} = Data.apply_operation(data, operation)
    end

    test "garbage collects input values that are no longer used" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()},
          {:set_input_value, @cid, "i1", "value"}
        ])

      operation = {:delete_cell, @cid, "c1"}

      empty_map = %{}

      assert {:ok, %{input_infos: ^empty_map}, actions} = Data.apply_operation(data, operation)

      assert {:clean_up_input_values, %{"i1" => %{value: "value", hash: :erlang.phash2("value")}}} in actions
    end

    test "marks cells bound to the deleted input as stale" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()},
          evaluate_cells_operations(["c2"], %{bind_inputs: %{"c2" => ["i1"]}})
        ])

      operation = {:delete_cell, @cid, "c1"}

      assert {:ok, %{cell_infos: %{"c2" => %{eval: %{validity: :stale}}}}, _actions} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :restore_cell" do
    test "returns an error if cell with the given id is not in the bin" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}}
        ])

      operation = {:restore_cell, @cid, "c1"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error if there are no sections to restore to" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:delete_section, @cid, "s1", true}
        ])

      operation = {:restore_cell, @cid, "c1"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "if the original section exists, restores cell at the index of deletion" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:delete_cell, @cid, "c2"}
        ])

      operation = {:restore_cell, @cid, "c2"}

      assert {:ok,
              %{
                notebook: %{
                  sections: [%{id: "s1", cells: [%{id: "c1"}, %{id: "c2"}, %{id: "c3"}]}]
                },
                cell_infos: %{"c2" => %{}},
                bin_entries: []
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "if the original section does not exist, restores cell at the end of the notebook" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:delete_section, @cid, "s1", true}
        ])

      operation = {:restore_cell, @cid, "c1"}

      assert {:ok,
              %{
                notebook: %{
                  sections: [%{id: "s2", cells: [%{id: "c2"}, %{id: "c1"}]}]
                },
                cell_infos: %{"c2" => %{}},
                bin_entries: []
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "given a smart cell starts it when there is a matching definition" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :smart, "c1", %{kind: "text"}},
          {:delete_cell, @cid, "c1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          {:set_smart_cell_definitions, @cid, @smart_cell_definitions}
        ])

      operation = {:restore_cell, @cid, "c1"}

      assert {:ok, %{cell_infos: %{"c1" => %{status: :starting}}},
              [{:start_smart_cell, %{id: "c1"}, %{id: "s1"}}]} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :move_cell" do
    test "returns an error given invalid cell id" do
      data = Data.new()
      operation = {:move_cell, @cid, "nonexistent", 1}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given no offset" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}}
        ])

      operation = {:move_cell, @cid, "c1", 0}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error if the cell is evaluating and would move to a different section" do
      # In practice we don't want evaluating cells to be moved between
      # a section and a branching section, however for simplicity we
      # do the same for other sections too

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation = {:move_cell, @cid, "c1", 1}
      assert :error = Data.apply_operation(data, operation)
    end

    test "given negative offset, moves the cell upwards" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:insert_cell, @cid, "s1", 3, :code, "c4", %{}}
        ])

      operation = {:move_cell, @cid, "c3", -1}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{cells: [%{id: "c1"}, %{id: "c3"}, %{id: "c2"}, %{id: "c4"}]}
                  ]
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "given positive offset, moves the cell downards" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:insert_cell, @cid, "s1", 3, :code, "c4", %{}}
        ])

      operation = {:move_cell, @cid, "c2", 1}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{
                      cells: [%{id: "c1"}, %{id: "c3"}, %{id: "c2"}, %{id: "c4"}]
                    }
                  ]
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "allows moving cells between sections" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s2", 0, :code, "c3", %{}},
          {:insert_cell, @cid, "s2", 1, :code, "c4", %{}}
        ])

      operation = {:move_cell, @cid, "c2", 1}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{cells: [%{id: "c1"}]},
                    %{cells: [%{id: "c2"}, %{id: "c3"}, %{id: "c4"}]}
                  ]
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "marks invalidated cells as stale" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:insert_cell, @cid, "s1", 3, :code, "c4", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3", "c4"],
            uses: %{"c2" => ["c1"], "c3" => ["c2"], "c4" => ["c1"]}
          )
        ])

      operation = {:move_cell, @cid, "c1", 1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :evaluated}},
                  "c2" => %{eval: %{validity: :stale}},
                  "c3" => %{eval: %{validity: :stale}},
                  "c4" => %{eval: %{validity: :evaluated}}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "cells with :unknown are marked as stale when new identifiers get into scope" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3"],
            uses: %{"c2" => :unknown}
          )
        ])

      operation = {:move_cell, @cid, "c3", -1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :evaluated}},
                  "c2" => %{eval: %{validity: :stale}},
                  "c3" => %{eval: %{validity: :evaluated}}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "moving a markdown cell does not change cell validity" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :markdown, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"])
        ])

      operation = {:move_cell, @cid, "c2", -1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :evaluated}}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "unqueues moved and child cells" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2", "c3"]}
        ])

      operation = {:move_cell, @cid, "c2", -1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{eval: %{status: :ready}},
                  "c3" => %{eval: %{status: :ready}}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "when moving to a branching section, marks dependent cells in other sections as stale" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c3", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c4", %{}},
          {:set_section_parent, @cid, "s2", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3", "c4"],
            uses: %{"c4" => ["c2"]}
          )
        ])

      operation = {:move_cell, @cid, "c2", 1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :evaluated}},
                  "c2" => %{eval: %{validity: :stale}},
                  "c3" => %{eval: %{validity: :evaluated}},
                  "c4" => %{eval: %{validity: :stale}}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "moving a cell back and forth doesn't impact validity" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3"],
            uses: %{"c2" => ["c1"], "c3" => ["c1"]}
          )
        ])

      {:ok, data_moved, []} = Data.apply_operation(data, {:move_cell, @cid, "c2", -1})
      {:ok, data_reversed, []} = Data.apply_operation(data_moved, {:move_cell, @cid, "c2", 1})

      assert data_reversed == data
    end
  end

  describe "apply_operation/2 given :move_section" do
    test "returns an error given invalid section id" do
      data = Data.new()
      operation = {:move_section, @cid, "nonexistent", 1}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given no offset" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_section, @cid, 1, "s2"}
        ])

      operation = {:move_section, @cid, "s2", 0}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error when moving a regular section below one of its child sections" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_section, @cid, 1, "s2"},
          {:insert_section, @cid, 2, "s3"},
          {:set_section_parent, @cid, "s2", "s1"}
        ])

      operation = {:move_section, @cid, "s1", 1}

      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error when moving a child section above its parent section" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_section, @cid, 1, "s2"},
          {:insert_section, @cid, 2, "s3"},
          {:set_section_parent, @cid, "s2", "s1"}
        ])

      operation = {:move_section, @cid, "s2", -1}

      assert :error = Data.apply_operation(data, operation)
    end

    test "given negative offset, moves the section upwards" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c3", %{}},
          {:insert_cell, @cid, "s2", 1, :code, "c4", %{}}
        ])

      operation = {:move_section, @cid, "s2", -1}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{cells: [%{id: "c3"}, %{id: "c4"}]},
                    %{cells: [%{id: "c1"}, %{id: "c2"}]}
                  ]
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "given positive offset, moves the section downwards" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c3", %{}},
          {:insert_cell, @cid, "s2", 1, :code, "c4", %{}}
        ])

      operation = {:move_section, @cid, "s1", 1}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{cells: [%{id: "c3"}, %{id: "c4"}]},
                    %{cells: [%{id: "c1"}, %{id: "c2"}]}
                  ]
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "marks invalidated cells as stale" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c3", %{}},
          {:insert_cell, @cid, "s3", 1, :code, "c4", %{}},
          {:insert_cell, @cid, "s3", 2, :code, "c5", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3", "c4", "c5"],
            uses: %{"c2" => ["c1"], "c3" => ["c1"], "c4" => ["c2"]}
          )
        ])

      operation = {:move_section, @cid, "s1", 1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :evaluated}},
                  "c2" => %{eval: %{validity: :stale}},
                  "c3" => %{eval: %{validity: :evaluated}},
                  "c4" => %{eval: %{validity: :stale}},
                  "c5" => %{eval: %{validity: :evaluated}}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "unqueues moved and child cells" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c3", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2", "c3"]}
        ])

      operation = {:move_section, @cid, "s2", -1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{eval: %{status: :ready}},
                  "c3" => %{eval: %{status: :ready}}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "does not invalidate any cells if a branching sections is moved" do
      # Cell 2 uses an identifier with a name defined in cell 3,
      # but it shouldn't see that identifier, because it branches
      # from cell 1

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c3", %{}},
          {:insert_section, @cid, 3, "s4"},
          {:insert_cell, @cid, "s4", 0, :code, "c4", %{}},
          {:set_section_parent, @cid, "s2", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3", "c4"],
            uses: %{"c2" => ["c3"]}
          )
        ])

      operation = {:move_section, @cid, "s2", 1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :evaluated}},
                  "c2" => %{eval: %{validity: :evaluated}},
                  "c3" => %{eval: %{validity: :evaluated}},
                  "c4" => %{eval: %{validity: :evaluated}}
                }
              }, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :queue_cells_evaluation" do
    test "returns an error given an empty list of cells" do
      data = Data.new()
      operation = {:queue_cells_evaluation, @cid, []}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given invalid cell id" do
      data = Data.new()
      operation = {:queue_cells_evaluation, @cid, ["nonexistent"]}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given non-code cell" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :markdown, "c1", %{}}
        ])

      operation = {:queue_cells_evaluation, @cid, ["c1"]}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error for an evaluating cell" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation = {:queue_cells_evaluation, @cid, ["c1"]}
      assert :error = Data.apply_operation(data, operation)
    end

    test "if there is no runtime and this is the first evaluation, returns a connect runtime action" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}}
        ])

      operation = {:queue_cells_evaluation, @cid, ["c1"]}

      assert {:ok,
              %{
                cell_infos: %{"c1" => %{eval: %{status: :queued}}},
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil}
                }
              } = new_data, [:connect_runtime]} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new(["c1"])
    end

    test "if runtime start has already been requested, just queues the cell" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation = {:queue_cells_evaluation, @cid, ["c2"]}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{status: :queued}},
                  "c2" => %{eval: %{status: :queued}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil}
                }
              } = new_data, []} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new(["c1", "c2"])
    end

    test "marks the cell as evaluating if the corresponding section is idle" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"])
        ])

      operation = {:queue_cells_evaluation, @cid, ["c1"]}

      assert {:ok,
              %{
                cell_infos: %{"c1" => %{eval: %{status: :evaluating}}},
                section_infos: %{
                  "s1" => %{evaluating_cell_id: "c1"}
                }
              } = new_data, _actions} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
    end

    test "returns start evaluation action if the corresponding section is idle" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"])
        ])

      operation = {:queue_cells_evaluation, @cid, ["c1"]}

      assert {:ok, _data, [{:start_evaluation, %{id: "c1"}, %{id: "s1"}}]} =
               Data.apply_operation(data, operation)
    end

    test "marks the cell as queued if the corresponding section is already evaluating" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation = {:queue_cells_evaluation, @cid, ["c2"]}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{eval: %{status: :queued}}},
                section_infos: %{
                  "s1" => %{evaluating_cell_id: "c1"}
                }
              } = new_data, []} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new(["c2"])
    end

    test "marks the cell as queued if a previous section is already evaluating" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation = {:queue_cells_evaluation, @cid, ["c2"]}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{eval: %{status: :queued}}},
                section_infos: %{
                  "s1" => %{evaluating_cell_id: "c1"},
                  "s2" => %{evaluating_cell_id: nil}
                }
              } = new_data, []} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s2"].evaluation_queue == MapSet.new(["c2"])
    end

    test "queues previous unevaluated and stale cells" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c3", %{}},
          {:insert_cell, @cid, "s2", 1, :code, "c4", %{}},
          # Evaluate first 2 cells
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2"], uses: %{"c2" => ["c1"]}),
          # Evaluate the first cell, so the second becomes stale
          evaluate_cells_operations(["c1"], versions: %{"c1" => 1})
        ])

      # The above leads to:
      #
      # Section 1:
      #   * cell 1 - evaluated
      #   * cell 2 - stale
      # Section 2:
      #   * cell 3 - fresh
      #   * cell 4 - fresh
      #
      # Queuing cell 4 should also queue cell 3 and cell 2, so that
      # they all become evaluated.

      operation = {:queue_cells_evaluation, @cid, ["c4"]}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{eval: %{status: :evaluating}},
                  "c3" => %{eval: %{status: :queued}},
                  "c4" => %{eval: %{status: :queued}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: "c2"},
                  "s2" => %{evaluating_cell_id: nil}
                }
              } = new_data, _actions} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s2"].evaluation_queue == MapSet.new(["c3", "c4"])
    end

    test "given a branch cell, queues branch parent cells" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c3", %{}},
          {:set_section_parent, @cid, "s3", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"])
        ])

      operation = {:queue_cells_evaluation, @cid, ["c3"]}

      # Cell 3 depends directly on cell 1, so cell 2 shouldn't be queued

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :fresh, status: :evaluating}},
                  "c2" => %{eval: %{validity: :fresh, status: :ready}},
                  "c3" => %{eval: %{validity: :fresh, status: :queued}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: "c1"},
                  "s2" => %{evaluating_cell_id: nil},
                  "s3" => %{evaluating_cell_id: nil}
                }
              } = new_data, _actions} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s2"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s3"].evaluation_queue == MapSet.new(["c3"])
    end

    test "marks first branch cell as queued if a regular section is evaluating" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c3", %{}},
          {:set_section_parent, @cid, "s2", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"]),
          {:queue_cells_evaluation, @cid, ["c3"]}
        ])

      operation = {:queue_cells_evaluation, @cid, ["c2"]}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{eval: %{status: :queued}},
                  "c3" => %{eval: %{status: :evaluating}}
                },
                section_infos: %{
                  "s2" => %{evaluating_cell_id: nil},
                  "s3" => %{evaluating_cell_id: "c3"}
                }
              } = new_data, _actions} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s2"].evaluation_queue == MapSet.new(["c2"])
      assert new_data.section_infos["s3"].evaluation_queue == MapSet.new([])
    end

    test "marks first branch cell as evaluating if no regular section is evaluating" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:set_section_parent, @cid, "s2", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"])
        ])

      operation = {:queue_cells_evaluation, @cid, ["c2"]}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{eval: %{status: :evaluating}}},
                section_infos: %{
                  "s2" => %{evaluating_cell_id: "c2"}
                }
              } = new_data,
              [{:start_evaluation, %{id: "c2"}, %{id: "s2"}}]} =
               Data.apply_operation(data, operation)

      assert new_data.section_infos["s2"].evaluation_queue == MapSet.new([])
    end

    test "marks the second branch cell as evaluating if the first one is evaluated, even if a regular section is evaluating" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_cell, @cid, "s2", 1, :code, "c3", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c4", %{}},
          {:set_section_parent, @cid, "s2", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2"]),
          {:queue_cells_evaluation, @cid, ["c4"]}
        ])

      operation = {:queue_cells_evaluation, @cid, ["c3"]}

      assert {:ok,
              %{
                cell_infos: %{
                  "c3" => %{eval: %{status: :evaluating}},
                  "c4" => %{eval: %{status: :evaluating}}
                },
                section_infos: %{
                  "s2" => %{evaluating_cell_id: "c3"},
                  "s3" => %{evaluating_cell_id: "c4"}
                }
              } = new_data, _actions} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s2"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s3"].evaluation_queue == MapSet.new([])
    end

    test "marks regular cell as evaluating if only a branch cell is evaluating" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c3", %{}},
          {:set_section_parent, @cid, "s2", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"]),
          {:queue_cells_evaluation, @cid, ["c2"]}
        ])

      operation = {:queue_cells_evaluation, @cid, ["c3"]}

      assert {:ok,
              %{
                cell_infos: %{"c3" => %{eval: %{status: :evaluating}}},
                section_infos: %{
                  "s3" => %{evaluating_cell_id: "c3"}
                }
              } = new_data, _actions} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s3"].evaluation_queue == MapSet.new([])
    end
  end

  describe "apply_operation/2 given :add_cell_evaluation_output" do
    test "updates the cell outputs" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation = {:add_cell_evaluation_output, @cid, "c1", {:stdout, "Hello!"}}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{
                      cells: [%{outputs: [{1, {:stdout, "Hello!"}}]}]
                    }
                  ]
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "adds output even after the cell finished evaluation" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"])
        ])

      operation = {:add_cell_evaluation_output, @cid, "c1", {:stdout, "Hello!"}}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{
                      cells: [%{outputs: [{2, {:stdout, "Hello!"}}, _result]}]
                    }
                  ]
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "sets dirty flag to true if outputs are persisted" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:set_notebook_attributes, @cid, %{persist_outputs: true}},
          {:notebook_saved, @cid, []}
        ])

      operation = {:add_cell_evaluation_output, @cid, "c1", {:stdout, "Hello!"}}

      assert {:ok, %{dirty: true}, []} = Data.apply_operation(data, operation)
    end

    test "stores default values for new inputs in the pre-evaluation data" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation = {:add_cell_evaluation_output, @cid, "c1", {:input, input}}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{
                    eval: %{data: %{input_infos: %{"i1" => %{value: "hey"}}}}
                  }
                }
              }, _} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :add_cell_evaluation_response" do
    test "updates the cell outputs" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c1", {:ok, [1, 2, 3]}, eval_meta()}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{
                      cells: [%{outputs: [{1, {:ok, [1, 2, 3]}}]}]
                    }
                  ]
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "marks the cell as ready" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta()}

      assert {:ok,
              %{
                cell_infos: %{"c1" => %{eval: %{status: :ready}}},
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil}
                }
              } = new_data, []} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
    end

    test "preserves validity status" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          # Evaluate the first cell
          evaluate_cells_operations(["c1"]),
          # Start evaluating the second cell
          {:queue_cells_evaluation, @cid, ["c2"]},
          # Remove the first cell, this should make the second cell stale
          {:delete_cell, @cid, "c1"}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c2", @eval_resp, eval_meta(uses: ["c1"])}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{eval: %{validity: :stale}}}
              }, []} = Data.apply_operation(data, operation)
    end

    test "marks next queued cell in this section as evaluating if there is one" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2"]}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta()}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{eval: %{status: :evaluating}}},
                section_infos: %{
                  "s1" => %{evaluating_cell_id: "c2"}
                }
              } = new_data,
              [{:start_evaluation, %{id: "c2"}, %{id: "s1"}}]} =
               Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
    end

    test "marks next queued cell in a further section as evaluating if there is one" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2"]}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta()}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{eval: %{status: :evaluating}}},
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil},
                  "s2" => %{evaluating_cell_id: "c2"}
                }
              } = new_data,
              [{:start_evaluation, %{id: "c2"}, %{id: "s2"}}]} =
               Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s2"].evaluation_queue == MapSet.new([])
    end

    test "marks evaluated dependent cells as stale" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c3", %{}},
          {:insert_cell, @cid, "s2", 1, :code, "c4", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3", "c4"],
            uses: %{"c2" => ["c1"], "c4" => ["c2"]}
          ),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation =
        {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta(defines: %{"c1" => 1})}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{eval: %{validity: :stale}},
                  "c3" => %{eval: %{validity: :evaluated}},
                  "c4" => %{eval: %{validity: :stale}}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "cells with :unknown are marked as stale whenever any identifier changes" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3"],
            uses: %{"c2" => :unknown}
          ),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation =
        {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta(defines: %{"c1" => 1})}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :evaluated}},
                  "c2" => %{eval: %{validity: :stale}},
                  "c3" => %{eval: %{validity: :evaluated}}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "updates evaluation queue to include invalidated cells necessary for those already queued" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:insert_cell, @cid, "s1", 3, :code, "c4", %{}},
          {:insert_cell, @cid, "s1", 4, :code, "c5", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3", "c4", "c5"],
            uses: %{"c2" => ["c1"], "c3" => ["c2"]}
          ),
          {:queue_cells_evaluation, @cid, ["c1", "c5"]}
        ])

      operation =
        {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta(defines: %{"c1" => 1})}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{status: :ready}},
                  "c2" => %{eval: %{status: :evaluating}},
                  "c3" => %{eval: %{status: :queued}},
                  "c4" => %{eval: %{status: :ready}},
                  "c5" => %{eval: %{status: :queued}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: "c2"}
                }
              } = new_data, _actions} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new(["c3", "c5"])
    end

    test "marks dependent evaluated child cells as stale in branch sections" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c3", %{}},
          {:insert_section, @cid, 3, "s4"},
          {:insert_cell, @cid, "s4", 0, :code, "c4", %{}},
          {:set_section_parent, @cid, "s3", "s2"},
          {:set_section_parent, @cid, "s4", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3", "c4"],
            uses: %{"c3" => ["c2"], "c4" => ["c1", "c2"]}
          ),
          {:queue_cells_evaluation, @cid, ["c2"]}
        ])

      operation =
        {:add_cell_evaluation_response, @cid, "c2", @eval_resp, eval_meta(defines: %{"c2" => 1})}

      # Section s4 is independent of section s2, so it shouldn't be invalidated

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :evaluated}},
                  "c2" => %{eval: %{validity: :evaluated}},
                  "c3" => %{eval: %{validity: :stale}},
                  "c4" => %{eval: %{validity: :evaluated}}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "marks invalidated automatically reevaluating cells for evaluation" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:set_cell_attributes, @cid, "c3", %{reevaluate_automatically: true}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3"],
            uses: %{"c2" => ["c1"], "c3" => ["c2"]}
          ),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation =
        {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta(defines: %{"c1" => 1})}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{eval: %{status: :evaluating}},
                  "c3" => %{eval: %{status: :queued}}
                }
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "does not mark child automatically reevaluating cells for evaluation if they are fresh" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_cell_attributes, @cid, "c2", %{reevaluate_automatically: true}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta()}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{status: :ready, validity: :evaluated}},
                  "c2" => %{eval: %{status: :ready, validity: :fresh}}
                }
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "unqueues child cells if the evaluation errored" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c3", %{}},
          {:insert_cell, @cid, "s2", 1, :code, "c4", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2", "c3", "c4"]}
        ])

      operation =
        {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta(errored: true)}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{eval: %{status: :ready, validity: :fresh}},
                  "c3" => %{eval: %{status: :ready, validity: :fresh}},
                  "c4" => %{eval: %{status: :ready, validity: :fresh}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil},
                  "s2" => %{evaluating_cell_id: nil}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "disables child cells automatic reevaluation if the evaluation errored" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:set_cell_attributes, @cid, "c3", %{reevaluate_automatically: true}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3"],
            uses: %{"c2" => ["c1"], "c3" => ["c2"]}
          ),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation =
        {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta(errored: true)}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{eval: %{status: :ready}},
                  "c3" => %{eval: %{status: :ready, reevaluates_automatically: false}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "re-enables child cells automatic reevaluation if errored evaluation is fixed" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:set_cell_attributes, @cid, "c3", %{reevaluate_automatically: true}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3"],
            uses: %{"c2" => ["c1"], "c3" => ["c2"]}
          ),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta(errored: true)},
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta()}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{eval: %{status: :evaluating}},
                  "c3" => %{eval: %{status: :queued, reevaluates_automatically: true}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: "c2"}
                }
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "if bound input value changes during cell evaluation, the cell is marked as stale afterwards" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2"]},
          {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()},
          {:add_cell_evaluation_response, @cid, "c2", @eval_resp, eval_meta()},
          # Make the code cell evaluating
          {:queue_cells_evaluation, @cid, ["c2"]},
          # Bind the input (effectively read the current value)
          {:bind_input, @cid, "c2", "i1"},
          # Change the input value, while the cell is evaluating
          {:set_input_value, @cid, "i1", "stuff"}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c2", @eval_resp, eval_meta()}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{eval: %{validity: :stale}}
                }
              }, _} = Data.apply_operation(data, operation)
    end

    test "updates evaluation time based on the result metadata" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c1", {:ok, [1, 2, 3]}, eval_meta()}

      assert {:ok,
              %{
                cell_infos: %{"c1" => %{eval: %{evaluation_time_ms: evaluation_time}}}
              }, []} = Data.apply_operation(data, operation)

      assert evaluation_time == 10
    end

    test "sets dirty flag to true if outputs are persisted" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:set_notebook_attributes, @cid, %{persist_outputs: true}},
          {:notebook_saved, @cid, []}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c1", {:ok, [1, 2, 3]}, eval_meta()}

      assert {:ok, %{dirty: true}, []} = Data.apply_operation(data, operation)
    end

    test "stores default values for new inputs" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()}

      assert {:ok, %{input_infos: %{"i1" => %{value: "hey"}}}, _} =
               Data.apply_operation(data, operation)
    end

    test "stores default values for new nested inputs" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}
      output = {:grid, [{:input, input}], %{}}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c1", output, eval_meta()}

      assert {:ok, %{input_infos: %{"i1" => %{value: "hey"}}}, _} =
               Data.apply_operation(data, operation)
    end

    test "keeps input values for inputs that existed" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()},
          {:set_input_value, @cid, "i1", "value"},
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      # Output the same input again
      operation = {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()}

      assert {:ok, %{input_infos: %{"i1" => %{value: "value"}}}, _} =
               Data.apply_operation(data, operation)
    end

    test "garbage collects input values that are no longer used" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()},
          {:set_input_value, @cid, "i1", "value"},
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      # This time w don't output the input
      operation = {:add_cell_evaluation_response, @cid, "c1", {:ok, 10}, eval_meta()}

      empty_map = %{}

      assert {:ok, %{input_infos: ^empty_map},
              [{:clean_up_input_values, %{"i1" => %{value: "value"}}}]} =
               Data.apply_operation(data, operation)
    end

    test "does not garbage collect inputs if present in another cell" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2"]},
          {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()},
          {:add_cell_evaluation_response, @cid, "c2", {:input, input}, eval_meta()},
          {:set_input_value, @cid, "i1", "value"},
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      # This time w don't output the input
      operation = {:add_cell_evaluation_response, @cid, "c1", {:ok, 10}, eval_meta()}

      assert {:ok, %{input_infos: %{"i1" => %{value: "value"}}}, _} =
               Data.apply_operation(data, operation)
    end

    test "does not garbage collect inputs if another evaluation is ongoing" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_section, @cid, 1, "s2"},
          {:insert_section, @cid, 2, "s3"},
          {:set_section_parent, @cid, "s2", "s1"},
          {:set_section_parent, @cid, "s3", "s1"},
          {:insert_cell, @cid, "s2", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s3", 0, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()},
          {:set_input_value, @cid, "i1", "value"},
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:queue_cells_evaluation, @cid, ["c2"]}
        ])

      # This time w don't output the input
      operation = {:add_cell_evaluation_response, @cid, "c1", {:ok, 10}, eval_meta()}

      assert {:ok, %{input_infos: %{"i1" => %{value: "value"}}}, _} =
               Data.apply_operation(data, operation)
    end

    test "evaluating code cell before smart cell changes its parents" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :smart, "c2", %{kind: "text"}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:set_smart_cell_definitions, @cid, @smart_cell_definitions},
          {:smart_cell_started, @cid, "c2", Delta.new(), nil, %{}, nil},
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta()}

      assert {:ok, %{},
              [
                {:set_smart_cell_parents, %{id: "c2"}, %{id: "s1"},
                 [{%{id: "c1"}, %{id: "s1"}}, {%{id: "setup"}, %{id: "setup-section"}}]}
              ]} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :add_cell_doctest_report" do
    test "returns an error given invalid cell id" do
      data = Data.new()
      operation = {:add_cell_doctest_report, @cid, "c1", %{status: :running, line: 5}}
      assert :error = Data.apply_operation(data, operation)
    end

    test "adds doctest report to cell evaluation info" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]}
        ])

      doctest_report = %{status: :running, line: 5}

      operation = {:add_cell_doctest_report, @cid, "c1", doctest_report}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{doctest_reports: %{5 => ^doctest_report}}}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "updates doctest report by line" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:add_cell_doctest_report, @cid, "c1", %{status: :running, line: 5}}
        ])

      doctest_report = %{status: :success, line: 5}

      operation = {:add_cell_doctest_report, @cid, "c1", doctest_report}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{doctest_reports: %{5 => ^doctest_report}}}
                }
              }, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :bind_input" do
    test "returns an error given invalid input cell id" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}}
        ])

      operation = {:bind_input, @cid, "c1", "nonexistent"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given non-input cell" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}}
        ])

      operation = {:bind_input, @cid, "c2", "c1"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "updates code cell info with binding to the input cell" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()},
          {:queue_cells_evaluation, @cid, ["c2"]}
        ])

      operation = {:bind_input, @cid, "c2", "i1"}

      bound_to_inputs = %{"i1" => :erlang.phash2("hey")}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{eval: %{new_bound_to_inputs: ^bound_to_inputs}}
                }
              }, _actions} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :reflect_main_evaluation_failure" do
    test "clears evaluation queue and marks evaluated and evaluating cells as aborted" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"]),
          {:queue_cells_evaluation, @cid, ["c2", "c3"]}
        ])

      operation = {:reflect_main_evaluation_failure, @cid}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :aborted, status: :ready}},
                  "c2" => %{eval: %{validity: :aborted, status: :ready}},
                  "c3" => %{eval: %{validity: :fresh, status: :ready}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil}
                }
              } = new_data, _actions} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
    end

    test "leaves branching sections unchanged" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_cell, @cid, "s2", 1, :code, "c3", %{}},
          {:set_section_parent, @cid, "s2", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2"]),
          {:queue_cells_evaluation, @cid, ["c3"]}
        ])

      operation = {:reflect_main_evaluation_failure, @cid}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :aborted, status: :ready}},
                  "c2" => %{eval: %{validity: :evaluated, status: :ready}},
                  "c3" => %{eval: %{validity: :fresh, status: :evaluating}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil},
                  "s2" => %{evaluating_cell_id: "c3"}
                }
              } = new_data, _actions} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s2"].evaluation_queue == MapSet.new([])
    end
  end

  describe "apply_operation/2 given :reflect_evaluation_failure" do
    test "clears section evaluation queue and marks evaluated and evaluating cells as aborted" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_cell, @cid, "s2", 1, :code, "c3", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c4", %{}},
          {:set_section_parent, @cid, "s2", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2"]),
          {:queue_cells_evaluation, @cid, ["c3", "c4"]}
        ])

      operation = {:reflect_evaluation_failure, @cid, "s2"}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :evaluated, status: :ready}},
                  "c2" => %{eval: %{validity: :aborted, status: :ready}},
                  "c3" => %{eval: %{validity: :aborted, status: :ready}},
                  "c4" => %{eval: %{validity: :fresh, status: :evaluating}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil},
                  "s2" => %{evaluating_cell_id: nil},
                  "s3" => %{evaluating_cell_id: "c4"}
                }
              } = new_data, _actions} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s2"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s3"].evaluation_queue == MapSet.new([])
    end
  end

  describe "apply_operation/2 given :cancel_cell_evaluation" do
    test "returns an error given invalid cell id" do
      data = Data.new()
      operation = {:cancel_cell_evaluation, @cid, "nonexistent"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error for an evaluated cell" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"])
        ])

      operation = {:cancel_cell_evaluation, @cid, "c1"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "if the cell is evaluating, clears all sections evaluation and queues" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c3", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"]),
          {:queue_cells_evaluation, @cid, ["c2", "c3"]}
        ])

      operation = {:cancel_cell_evaluation, @cid, "c2"}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :aborted, status: :ready}},
                  "c2" => %{eval: %{validity: :aborted, status: :ready}},
                  "c3" => %{eval: %{validity: :fresh, status: :ready}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil},
                  "s2" => %{evaluating_cell_id: nil}
                }
              } = new_data, _actions} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s2"].evaluation_queue == MapSet.new([])
    end

    test "if the cell is evaluating, returns stop evaluation action" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2"]}
        ])

      operation = {:cancel_cell_evaluation, @cid, "c1"}

      assert {:ok, _data, [{:stop_evaluation, %{id: "s1"}}]} =
               Data.apply_operation(data, operation)
    end

    test "if the cell is evaluating within branched section, clears this section evaluation and queue" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_cell, @cid, "s2", 1, :code, "c3", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c4", %{}},
          {:set_section_parent, @cid, "s2", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"]),
          {:queue_cells_evaluation, @cid, ["c2", "c3", "c4"]}
        ])

      operation = {:cancel_cell_evaluation, @cid, "c2"}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :evaluated, status: :ready}},
                  "c2" => %{eval: %{validity: :aborted, status: :ready}},
                  "c3" => %{eval: %{validity: :fresh, status: :ready}},
                  "c4" => %{eval: %{validity: :fresh, status: :evaluating}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil},
                  "s2" => %{evaluating_cell_id: nil},
                  "s3" => %{evaluating_cell_id: "c4"}
                }
              } = new_data, _actions} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s2"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s3"].evaluation_queue == MapSet.new([])
    end

    test "if the cell is queued, unqueues it" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2"]}
        ])

      operation = {:cancel_cell_evaluation, @cid, "c2"}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{eval: %{validity: :fresh, status: :ready}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: "c1"}
                }
              } = new_data, []} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
    end

    test "if the cell is queued, unqueues child cells that are also queued" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2", "c3"]}
        ])

      operation = {:cancel_cell_evaluation, @cid, "c2"}

      assert {:ok,
              %{
                cell_infos: %{"c3" => %{eval: %{status: :ready}}},
                section_infos: %{
                  "s1" => %{evaluating_cell_id: "c1"}
                }
              } = new_data, []} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
    end
  end

  describe "apply_operation/2 given :smart_cell_started" do
    test "returns an error if the cell is dead" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :smart, "c1", %{kind: "text"}}
        ])

      operation = {:smart_cell_started, @cid, "c1", Delta.new(), nil, %{}, nil}

      assert :error = Data.apply_operation(data, operation)
    end

    test "marks the cell as alive and updates its source" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          {:set_smart_cell_definitions, @cid, @smart_cell_definitions},
          {:insert_cell, @cid, "s1", 0, :smart, "c1", %{kind: "text"}}
        ])

      delta = Delta.new() |> Delta.insert("content")

      operation = {:smart_cell_started, @cid, "c1", delta, nil, %{}, nil}

      assert {:ok, %{cell_infos: %{"c1" => %{status: :started}}}, _actions} =
               Data.apply_operation(data, operation)
    end

    test "updates the cell source and returns a broadcast delta action" do
      client_id = "client1"

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          {:set_smart_cell_definitions, @cid, @smart_cell_definitions},
          {:insert_cell, @cid, "s1", 0, :smart, "c1", %{kind: "text"}}
        ])

      delta = Delta.new() |> Delta.insert("content")

      operation = {:smart_cell_started, client_id, "c1", delta, nil, %{}, nil}

      assert {:ok,
              %{
                notebook: %{sections: [%{cells: [%{id: "c1", source: "content"}]}]}
              },
              [{:report_delta, ^client_id, _cell, :primary, ^delta}]} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :update_smart_cell" do
    test "updates the cell attrs, source and returns a broadcast delta action" do
      client_id = "client1"

      delta1 = Delta.new() |> Delta.insert("content")

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          {:set_smart_cell_definitions, @cid, @smart_cell_definitions},
          {:insert_cell, @cid, "s1", 0, :smart, "c1", %{kind: "text"}},
          {:smart_cell_started, @cid, "c1", delta1, nil, %{}, nil}
        ])

      attrs = %{"text" => "content!"}
      delta2 = Delta.new() |> Delta.retain(7) |> Delta.insert("!")
      operation = {:update_smart_cell, client_id, "c1", attrs, delta2, nil}

      assert {:ok,
              %{
                notebook: %{
                  sections: [%{cells: [%{id: "c1", source: "content!", attrs: ^attrs}]}]
                }
              },
              [{:report_delta, ^client_id, _cell, :primary, ^delta2}]} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :queue_smart_cell_reevaluation" do
    test "returns error if the cell is fresh" do
      client_id = "client1"

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:set_smart_cell_definitions, @cid, @smart_cell_definitions},
          {:insert_cell, @cid, "s1", 0, :smart, "c1", %{kind: "text"}},
          {:smart_cell_started, @cid, "c1", Delta.new(), nil, %{}, nil}
        ])

      operation = {:queue_smart_cell_reevaluation, client_id, "c1"}

      assert :error = Data.apply_operation(data, operation)
    end

    test "queues the cell when already evaluated" do
      client_id = "client1"

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:set_smart_cell_definitions, @cid, @smart_cell_definitions},
          {:insert_cell, @cid, "s1", 0, :smart, "c1", %{kind: "text"}},
          {:smart_cell_started, @cid, "c1", Delta.new(), nil, %{}, nil},
          evaluate_cells_operations(["c1"])
        ])

      operation = {:queue_smart_cell_reevaluation, client_id, "c1"}

      assert {:ok,
              %{
                cell_infos: %{"c1" => %{eval: %{status: :evaluating}}}
              }, _actions} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :smart_cell_down" do
    test "updates the smart cell status" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          {:set_smart_cell_definitions, @cid, @smart_cell_definitions},
          {:insert_cell, @cid, "s1", 0, :smart, "c1", %{kind: "text"}}
        ])

      operation = {:smart_cell_down, @cid, "c1"}

      assert {:ok, %{cell_infos: %{"c1" => %{status: :down}}}, _actions} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :recover_smart_cell" do
    test "returns an error if the smart cell is not down" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          {:set_smart_cell_definitions, @cid, @smart_cell_definitions},
          {:insert_cell, @cid, "s1", 0, :smart, "c1", %{kind: "text"}}
        ])

      operation = {:recover_smart_cell, @cid, "c1"}

      assert :error = Data.apply_operation(data, operation)
    end

    test "marks the smart cell as starting if there is a runtime" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          {:set_smart_cell_definitions, @cid, @smart_cell_definitions},
          {:insert_cell, @cid, "s1", 0, :smart, "c1", %{kind: "text"}},
          {:smart_cell_down, @cid, "c1"}
        ])

      operation = {:recover_smart_cell, @cid, "c1"}

      assert {:ok, %{cell_infos: %{"c1" => %{status: :starting}}},
              [{:start_smart_cell, %{id: "c1"}, %{id: "s1"}}]} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :erase_outputs" do
    test "clears all sections evaluation and queues" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c3", %{}},
          {:set_section_parent, @cid, "s2", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"]),
          {:queue_cells_evaluation, @cid, ["c2", "c3"]}
        ])

      operation = {:erase_outputs, @cid}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :fresh, status: :ready}},
                  "c2" => %{eval: %{validity: :fresh, status: :ready}},
                  "c3" => %{eval: %{validity: :fresh, status: :ready}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil},
                  "s2" => %{evaluating_cell_id: nil}
                }
              } = new_data, _actions} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s2"].evaluation_queue == MapSet.new([])
    end

    test "removes code cell outputs" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :markdown, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c3"])
        ])

      operation = {:erase_outputs, @cid}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{
                      id: "s1",
                      cells: [
                        %{id: "c1", outputs: []},
                        %{id: "c2"},
                        %{id: "c3", outputs: []}
                      ]
                    }
                  ]
                }
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "removes doctest reports" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"]),
          {:add_cell_doctest_report, @cid, "c1", %{status: :running, line: 5}}
        ])

      operation = {:erase_outputs, @cid}

      empty_map = %{}

      assert {:ok, %{cell_infos: %{"c1" => %{eval: %{doctest_reports: ^empty_map}}}}, []} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :set_notebook_name" do
    test "updates notebook name with the given string" do
      data = Data.new()

      operation = {:set_notebook_name, @cid, "Cat's guide to life"}

      assert {:ok, %{notebook: %{name: "Cat's guide to life"}}, []} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :set_section_name" do
    test "returns an error given invalid cell id" do
      data = Data.new()
      operation = {:set_section_name, @cid, "nonexistent", "Chapter 1"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "updates section name with the given string" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"}
        ])

      operation = {:set_section_name, @cid, "s1", "Cat's guide to life"}

      assert {:ok, %{notebook: %{sections: [%{name: "Cat's guide to life"}]}}, []} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :client_join" do
    test "returns an error if the given process is already a client" do
      user = User.new()

      data =
        data_after_operations!([
          {:client_join, @cid, user}
        ])

      operation = {:client_join, @cid, user}
      assert :error = Data.apply_operation(data, operation)
    end

    test "adds the given process and user to their corresponding maps" do
      client_id = "client1"
      %{id: user_id} = user = User.new()
      data = Data.new()

      operation = {:client_join, client_id, user}

      assert {:ok, %{clients_map: %{^client_id => ^user_id}, users_map: %{^user_id => ^user}}, []} =
               Data.apply_operation(data, operation)
    end

    test "adds new entry to the cell revisions map for the client with the latest revision" do
      client1_id = "cid1"
      user = User.new()
      delta1 = Delta.new() |> Delta.insert("cats")

      data =
        data_after_operations!([
          {:client_join, client1_id, user},
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:apply_cell_delta, client1_id, "c1", :primary, delta1, 1}
        ])

      client2_id = "cid2"
      operation = {:client_join, client2_id, user}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{sources: %{primary: %{revision_by_client_id: %{^client2_id => 1}}}}
                }
              }, _} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :client_leave" do
    test "returns an error if the given process is not a client" do
      data = Data.new()

      operation = {:client_leave, @cid}
      assert :error = Data.apply_operation(data, operation)
    end

    test "removes the given process from the client map" do
      data =
        data_after_operations!([
          {:client_join, @cid, User.new()}
        ])

      operation = {:client_leave, @cid}

      empty_map = %{}
      assert {:ok, %{clients_map: ^empty_map}, []} = Data.apply_operation(data, operation)
    end

    test "removes the corresponding user from users map if it has no more client processes" do
      data =
        data_after_operations!([
          {:client_join, @cid, User.new()}
        ])

      operation = {:client_leave, @cid}

      empty_map = %{}
      assert {:ok, %{users_map: ^empty_map}, []} = Data.apply_operation(data, operation)
    end

    test "leaves the corresponding user in users map if it has more client processes" do
      %{id: user_id} = user = User.new()
      client1_id = "cid1"
      client2_id = "cid2"

      data =
        data_after_operations!([
          {:client_join, client1_id, user},
          {:client_join, client2_id, user}
        ])

      operation = {:client_leave, client2_id}

      assert {:ok, %{users_map: %{^user_id => ^user}}, []} = Data.apply_operation(data, operation)
    end

    test "removes an entry in the the cell revisions map for the client and purges deltas" do
      client1_id = "cid1"
      client2_id = "cid2"

      delta1 = Delta.new() |> Delta.insert("cats")

      data =
        data_after_operations!([
          {:client_join, client1_id, User.new()},
          {:client_join, client2_id, User.new()},
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:apply_cell_delta, client1_id, "c1", :primary, delta1, 1}
        ])

      operation = {:client_leave, client2_id}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{
                    sources: %{
                      primary: %{deltas: [], revision_by_client_id: revision_by_client_id}
                    }
                  }
                }
              }, _} = Data.apply_operation(data, operation)

      assert revision_by_client_id == %{client1_id => 1}
    end
  end

  describe "apply_operation/2 given :update_user" do
    test "returns an error if the given user is not a client" do
      data = Data.new()

      operation = {:update_user, @cid, User.new()}
      assert :error = Data.apply_operation(data, operation)
    end

    test "updates users map" do
      %{id: user_id} = user = User.new()

      data =
        data_after_operations!([
          {:client_join, @cid, user}
        ])

      updated_user = %{user | name: "Jake Peralta"}
      operation = {:update_user, @cid, updated_user}

      assert {:ok, %{users_map: %{^user_id => ^updated_user}}, []} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :apply_cell_delta" do
    test "returns an error given invalid cell id" do
      data =
        data_after_operations!([
          {:client_join, @cid, User.new()}
        ])

      operation = {:apply_cell_delta, @cid, "nonexistent", :primary, Delta.new(), 1}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given invalid revision" do
      data =
        data_after_operations!([
          {:client_join, @cid, User.new()},
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}}
        ])

      delta = Delta.new() |> Delta.insert("cats")
      operation = {:apply_cell_delta, @cid, "c1", :primary, delta, 5}

      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given non-joined client pid and older revision" do
      client1_id = "cid1"

      delta1 = Delta.new() |> Delta.insert("cats")

      data =
        data_after_operations!([
          {:client_join, client1_id, User.new()},
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:apply_cell_delta, client1_id, "c1", :primary, delta1, 1}
        ])

      delta = Delta.new() |> Delta.insert("cats")
      operation = {:apply_cell_delta, @cid, "c1", :primary, delta, 1}
      assert :error = Data.apply_operation(data, operation)
    end

    test "updates cell source according to the given delta" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}}
        ])

      delta = Delta.new() |> Delta.insert("cats")
      operation = {:apply_cell_delta, @cid, "c1", :primary, delta, 1}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{cells: [%{source: "cats"}]}
                  ]
                },
                cell_infos: %{"c1" => %{sources: %{primary: %{revision: 1}}}}
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "transforms the delta if the revision is not the most recent" do
      client1_id = "cid1"
      client2_id = "cid2"

      delta1 = Delta.new() |> Delta.insert("cats")

      data =
        data_after_operations!([
          {:client_join, client1_id, User.new()},
          {:client_join, client2_id, User.new()},
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:apply_cell_delta, client1_id, "c1", :primary, delta1, 1}
        ])

      delta2 = Delta.new() |> Delta.insert("tea")
      operation = {:apply_cell_delta, client2_id, "c1", :primary, delta2, 1}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{cells: [%{source: "catstea"}]}
                  ]
                },
                cell_infos: %{"c1" => %{sources: %{primary: %{revision: 2}}}}
              }, _} = Data.apply_operation(data, operation)
    end

    test "returns broadcast delta action with the transformed delta" do
      client1_id = "cid1"
      client2_id = "cid2"

      delta1 = Delta.new() |> Delta.insert("cats")

      data =
        data_after_operations!([
          {:client_join, client1_id, User.new()},
          {:client_join, client2_id, User.new()},
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:apply_cell_delta, client1_id, "c1", :primary, delta1, 1}
        ])

      delta2 = Delta.new() |> Delta.insert("tea")
      operation = {:apply_cell_delta, client2_id, "c1", :primary, delta2, 1}

      transformed_delta2 = Delta.new() |> Delta.retain(4) |> Delta.insert("tea")

      assert {:ok, _data, [{:report_delta, ^client2_id, _cell, :primary, ^transformed_delta2}]} =
               Data.apply_operation(data, operation)
    end

    test "given single client, does not keep deltas" do
      client_id = "client1"

      data =
        data_after_operations!([
          {:client_join, client_id, User.new()},
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}}
        ])

      delta = Delta.new() |> Delta.insert("cats")
      operation = {:apply_cell_delta, client_id, "c1", :primary, delta, 1}

      assert {:ok,
              %{
                cell_infos: %{"c1" => %{sources: %{primary: %{deltas: []}}}}
              }, _} = Data.apply_operation(data, operation)
    end

    test "given multiple client, keeps the delta" do
      client1_id = "cid1"
      client2_id = "cid2"

      data =
        data_after_operations!([
          {:client_join, client1_id, User.new()},
          {:client_join, client2_id, User.new()},
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}}
        ])

      delta = Delta.new() |> Delta.insert("cats")
      operation = {:apply_cell_delta, client1_id, "c1", :primary, delta, 1}

      assert {:ok,
              %{
                cell_infos: %{"c1" => %{sources: %{primary: %{deltas: [^delta]}}}}
              }, _} = Data.apply_operation(data, operation)
    end

    test "updates smart cell editor source given a secondary source delta" do
      data =
        data_after_operations!([
          {:client_join, @cid, User.new()},
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 1, :smart, "c1", %{kind: "text"}},
          {:set_runtime, @cid, connected_noop_runtime()},
          {:set_smart_cell_definitions, @cid, @smart_cell_definitions},
          {:smart_cell_started, @cid, "c1", Delta.new(), nil, %{},
           %{language: "text", placement: :bottom, source: ""}}
        ])

      delta = Delta.new() |> Delta.insert("cats")
      operation = {:apply_cell_delta, @cid, "c1", :secondary, delta, 1}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{cells: [%{editor: %{source: "cats"}}]}
                  ]
                },
                cell_infos: %{"c1" => %{sources: %{secondary: %{revision: 1}}}}
              }, _actions} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :report_cell_revision" do
    test "returns an error given invalid cell id" do
      data =
        data_after_operations!([
          {:client_join, @cid, User.new()}
        ])

      operation = {:report_cell_revision, @cid, "nonexistent", :primary, 1}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given non-joined client pid" do
      client1_id = "cid1"
      client2_id = "cid2"

      data =
        data_after_operations!([
          {:client_join, client1_id, User.new()},
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:apply_cell_delta, client1_id, "c1", :primary, Delta.new(insert: "cats"), 1}
        ])

      operation = {:report_cell_revision, client2_id, "c1", :primary, 1}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given invalid revision" do
      data =
        data_after_operations!([
          {:client_join, @cid, User.new()},
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}}
        ])

      operation = {:report_cell_revision, @cid, "c1", :primary, 1}
      assert :error = Data.apply_operation(data, operation)
    end

    test "updates client entry in the revisions map and purges unnecessary deltas" do
      client1_id = "cid1"
      client2_id = "cid2"

      delta1 = Delta.new() |> Delta.insert("cats")

      data =
        data_after_operations!([
          {:client_join, client1_id, User.new()},
          {:client_join, client2_id, User.new()},
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:apply_cell_delta, client1_id, "c1", :primary, delta1, 1}
        ])

      operation = {:report_cell_revision, client2_id, "c1", :primary, 1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{
                    sources: %{
                      primary: %{
                        deltas: [],
                        revision_by_client_id: %{^client1_id => 1, ^client2_id => 1}
                      }
                    }
                  }
                }
              }, _} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :set_cell_attributes" do
    test "returns an error given invalid cell id" do
      data = Data.new()

      operation = {:set_cell_attributes, @cid, "nonexistent", %{}}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given an unknown attribute key" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}}
        ])

      attrs = %{unknown: :value}
      operation = {:set_cell_attributes, @cid, "c1", attrs}

      assert :error = Data.apply_operation(data, operation)
    end

    test "updates cell with the given attributes" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}}
        ])

      attrs = %{disable_formatting: true, reevaluate_automatically: true}
      operation = {:set_cell_attributes, @cid, "c1", attrs}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{cells: [%{disable_formatting: true, reevaluate_automatically: true}]}
                  ]
                }
              }, _} = Data.apply_operation(data, operation)
    end

    test "setting reevaluate_automatically on stale cell marks it for evaluation" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2"], uses: %{"c2" => ["c1"]}),
          evaluate_cells_operations(["c1"], versions: %{"c1" => 1})
        ])

      attrs = %{reevaluate_automatically: true}
      operation = {:set_cell_attributes, @cid, "c2", attrs}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{status: :ready}},
                  "c2" => %{eval: %{status: :evaluating}}
                }
              }, _} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :set_input_value" do
    test "returns an error given invalid input id" do
      data = Data.new()

      operation = {:set_input_value, @cid, "nonexistent", "stuff"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "stores new input value" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()}
        ])

      operation = {:set_input_value, @cid, "i1", "stuff"}

      assert {:ok, %{input_infos: %{"i1" => %{value: "stuff"}}}, _} =
               Data.apply_operation(data, operation)
    end

    test "given input value change, marks evaluated bound cells and their dependents as stale" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          # Insert three evaluated cells and bind the second one to the input
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:insert_cell, @cid, "s1", 3, :code, "c4", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()},
          evaluate_cells_operations(["c2", "c3", "c4"],
            bind_inputs: %{"c3" => ["i1"]},
            uses: %{"c2" => ["c1"], "c3" => ["c2"], "c4" => ["c3"]}
          )
        ])

      operation = {:set_input_value, @cid, "i1", "stuff"}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{eval: %{validity: :evaluated}},
                  "c3" => %{eval: %{validity: :stale}},
                  "c4" => %{eval: %{validity: :stale}}
                }
              }, _} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :set_runtime" do
    test "updates data with the given runtime" do
      data = Data.new()

      runtime = connected_noop_runtime()
      operation = {:set_runtime, @cid, runtime}

      assert {:ok, %{runtime: ^runtime}, []} = Data.apply_operation(data, operation)
    end

    test "clears all statuses and the per-section queues" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c3", %{}},
          {:insert_cell, @cid, "s2", 1, :code, "c4", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2", "c3", "c4"]}
        ])

      runtime = connected_noop_runtime()
      operation = {:set_runtime, @cid, runtime}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{eval: %{validity: :aborted, status: :ready}},
                  "c2" => %{eval: %{validity: :fresh, status: :ready}},
                  "c3" => %{eval: %{validity: :fresh, status: :ready}},
                  "c4" => %{eval: %{validity: :fresh, status: :ready}}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil},
                  "s2" => %{evaluating_cell_id: nil}
                }
              } = new_data, []} = Data.apply_operation(data, operation)

      assert new_data.section_infos["s1"].evaluation_queue == MapSet.new([])
      assert new_data.section_infos["s2"].evaluation_queue == MapSet.new([])
    end

    test "starts evaluation if there was no runtime before and there is now" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:queue_cells_evaluation, @cid, ["setup"]}
        ])

      runtime = connected_noop_runtime()
      operation = {:set_runtime, @cid, runtime}

      assert {:ok,
              %{
                cell_infos: %{
                  "setup" => %{eval: %{status: :evaluating}}
                },
                section_infos: %{
                  "setup-section" => %{evaluating_cell_id: "setup"}
                }
              } = new_data,
              [{:start_evaluation, %{id: "setup"}, %{id: "setup-section"}}]} =
               Data.apply_operation(data, operation)

      assert new_data.section_infos["setup-section"].evaluation_queue == MapSet.new([])
    end
  end

  describe "apply_operation/2 given :set_smart_cell_definitions" do
    test "sets the definitions and starts dead cells with matching kinds" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :smart, "c1", %{kind: "text"}},
          {:set_runtime, @cid, connected_noop_runtime()}
        ])

      operation = {:set_smart_cell_definitions, @cid, @smart_cell_definitions}

      assert {:ok, %{cell_infos: %{"c1" => %{status: :starting}}}, _actions} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :set_file" do
    test "updates data with the given path" do
      data = Data.new()

      file = Livebook.FileSystem.File.local(p("/path/to/file.livemd"))
      operation = {:set_file, @cid, file}

      assert {:ok, %{file: ^file}, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :notebook_saved" do
    test "sets dirty flag to false" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"}
        ])

      operation = {:notebook_saved, @cid, []}

      assert {:ok, %{dirty: false}, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :set_secret" do
    test "adds secret and updates hub secret names" do
      data = Data.new()

      secret = Livebook.Factory.insert_secret(name: "SET_SECRET_SECRET", value: "value")

      operation = {:set_secret, @cid, secret}

      assert {:ok,
              %{
                secrets: %{"SET_SECRET_SECRET" => ^secret},
                notebook: %{hub_secret_names: ["SET_SECRET_SECRET"]}
              }, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :unset_secret" do
    test "removes secret and updates hub secret names" do
      secret = Livebook.Factory.insert_secret(name: "SET_SECRET_SECRET", value: "value")

      data =
        data_after_operations!([
          {:set_secret, @cid, secret}
        ])

      operation = {:unset_secret, @cid, secret.name}

      empty_map = %{}

      assert {:ok,
              %{
                secrets: ^empty_map,
                notebook: %{hub_secret_names: []}
              }, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :add_file_entries" do
    test "adds file entries to the notebook" do
      data = Data.new()

      file_entry1 = %{type: :attachment, name: "image.jpg"}
      file_entry2 = %{type: :url, name: "data.csv", url: "https://example.com/data.csv"}

      operation = {:add_file_entries, @cid, [file_entry1, file_entry2]}

      assert {:ok, %{notebook: %{file_entries: [^file_entry1, ^file_entry2]}}, []} =
               Data.apply_operation(data, operation)
    end

    test "replaces existing file entries with the same names" do
      file_entry1 = %{type: :attachment, name: "image.jpg"}
      file_entry2 = %{type: :url, name: "data.csv", url: "https://example.com/data.csv"}

      data =
        data_after_operations!([
          {:add_file_entries, @cid, [file_entry1, file_entry2]}
        ])

      file_entry3 = %{type: :attachment, name: "data.csv"}
      file_entry4 = %{type: :attachment, name: "document.pdf"}

      operation = {:add_file_entries, @cid, [file_entry3, file_entry4]}

      assert {:ok, %{notebook: %{file_entries: [^file_entry3, ^file_entry4, ^file_entry1]}}, []} =
               Data.apply_operation(data, operation)
    end

    test "removes matching file entry names from quarantine" do
      file = Livebook.FileSystem.File.new(Livebook.FileSystem.Local.new(), p("/image.jpg"))

      file_entry = %{type: :file, name: "image.jpg", file: file}

      notebook = %{
        Notebook.new()
        | file_entries: [file_entry],
          quarantine_file_entry_names: MapSet.new(["image.jpg"])
      }

      data = Data.new(notebook: notebook)

      file_entry = %{type: :attachment, name: "image.jpg"}

      operation = {:add_file_entries, @cid, [file_entry]}

      assert {:ok, %{notebook: notebook}, []} = Data.apply_operation(data, operation)
      assert notebook.quarantine_file_entry_names == MapSet.new()
    end
  end

  describe "apply_operation/2 given :delete_file_entry" do
    test "returns an error if no file entry with the given name exists" do
      data = Data.new()
      operation = {:delete_file_entry, @cid, "image.jpg"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "removes file entry from the notebook" do
      file_entry1 = %{type: :attachment, name: "image.jpg"}
      file_entry2 = %{type: :url, name: "data.csv", url: "https://example.com/data.csv"}

      data =
        data_after_operations!([
          {:add_file_entries, @cid, [file_entry1, file_entry2]}
        ])

      operation = {:delete_file_entry, @cid, "image.jpg"}

      assert {:ok, %{notebook: %{file_entries: [^file_entry2]}}, []} =
               Data.apply_operation(data, operation)
    end

    test "removes matching file entry names from quarantine" do
      file = Livebook.FileSystem.File.new(Livebook.FileSystem.Local.new(), p("/image.jpg"))

      file_entry = %{type: :file, name: "image.jpg", file: file}

      notebook = %{
        Notebook.new()
        | file_entries: [file_entry],
          quarantine_file_entry_names: MapSet.new(["image.jpg"])
      }

      data = Data.new(notebook: notebook)

      operation = {:delete_file_entry, @cid, "image.jpg"}

      assert {:ok, %{notebook: notebook}, []} = Data.apply_operation(data, operation)
      assert notebook.quarantine_file_entry_names == MapSet.new()
    end
  end

  describe "apply_operation/2 given :allow_file_entry" do
    test "returns an error if no file entry with the given name exists" do
      data = Data.new()
      operation = {:allow_file_entry, @cid, "image.jpg"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "removes matching file entry names from quarantine" do
      file = Livebook.FileSystem.File.new(Livebook.FileSystem.Local.new(), p("/image.jpg"))

      file_entry = %{type: :file, name: "image.jpg", file: file}

      notebook = %{
        Notebook.new()
        | file_entries: [file_entry],
          quarantine_file_entry_names: MapSet.new(["image.jpg"])
      }

      data = Data.new(notebook: notebook)

      operation = {:allow_file_entry, @cid, "image.jpg"}

      assert {:ok, %{notebook: notebook}, []} = Data.apply_operation(data, operation)
      assert notebook.quarantine_file_entry_names == MapSet.new()
    end
  end

  describe "apply_operation/2 given :set_app_settings" do
    test "updates notebook app settings" do
      data = Data.new()

      settings = %{Notebook.AppSettings.new() | slug: "new-slug"}
      operation = {:set_app_settings, @cid, settings}

      assert {:ok, %{notebook: %{app_settings: ^settings}}, []} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :set_deployed_app_slug" do
    test "updates deployed app slug" do
      settings = %{Notebook.AppSettings.new() | slug: "new-slug"}

      data =
        data_after_operations!([
          {:set_app_settings, @cid, settings}
        ])

      operation = {:set_deployed_app_slug, @cid, "new-slug"}

      assert {:ok, %{deployed_app_slug: "new-slug"}, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :app_deactivate" do
    test "returns an error if not in app mode" do
      data = Data.new()
      operation = {:app_deactivate, @cid}
      assert :error = Data.apply_operation(data, operation)
    end

    test "updates app status" do
      data = Data.new(mode: :app)

      operation = {:app_deactivate, @cid}

      assert {:ok, %{app_data: %{status: %{lifecycle: :deactivated}}}, [:app_report_status]} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :app_shutdown" do
    test "returns an error if not in app mode" do
      data = Data.new()
      operation = {:app_shutdown, @cid}
      assert :error = Data.apply_operation(data, operation)
    end

    test "updates app status" do
      data =
        data_after_operations!(Data.new(mode: :app), [
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"])
        ])

      operation = {:app_shutdown, @cid}

      assert {:ok, %{app_data: %{status: %{lifecycle: :shutting_down}}},
              [:app_report_status, :app_terminate]} = Data.apply_operation(data, operation)
    end

    test "does not return terminate action if there are clients" do
      data =
        data_after_operations!(Data.new(mode: :app), [
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:client_join, @cid, User.new()}
        ])

      operation = {:app_shutdown, @cid}

      assert {:ok, %{app_data: %{status: %{lifecycle: :shutting_down}}}, [:app_report_status]} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 app status transitions" do
    test "keeps status as :executing when an intermediate evaluation finishes" do
      data =
        data_after_operations!(Data.new(mode: :app), [
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2"]}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta()}

      assert {:ok, %{app_data: %{status: %{execution: :executing}}}, _actions} =
               Data.apply_operation(data, operation)
    end

    test "changes status to :error when an evaluation fails" do
      data =
        data_after_operations!(Data.new(mode: :app), [
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2"]}
        ])

      operation =
        {:add_cell_evaluation_response, @cid, "c1", @eval_resp, eval_meta(errored: true)}

      assert {:ok, %{app_data: %{status: %{execution: :error}}}, [:app_report_status]} =
               Data.apply_operation(data, operation)
    end

    test "changes status to :executed when all evaluation finishes" do
      data =
        data_after_operations!(Data.new(mode: :app), [
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"]),
          {:queue_cells_evaluation, @cid, ["c2"]}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c2", @eval_resp, eval_meta()}

      assert {:ok, %{app_data: %{status: %{execution: :executed}}}, [:app_report_status]} =
               Data.apply_operation(data, operation)
    end

    test "changes status to :error when evaluation is aborted and returns recover action" do
      data =
        data_after_operations!(Data.new(mode: :app), [
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2"])
        ])

      operation = {:reflect_main_evaluation_failure, @cid}

      assert {:ok, %{app_data: %{status: %{execution: :error}}},
              [:app_report_status, :app_recover]} = Data.apply_operation(data, operation)
    end

    test "changes status to :error when evaluation finishes with error" do
      data =
        data_after_operations!(Data.new(mode: :app), [
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"]),
          {:queue_cells_evaluation, @cid, ["c2"]}
        ])

      operation =
        {:add_cell_evaluation_response, @cid, "c2", @eval_resp, eval_meta(errored: true)}

      assert {:ok, %{app_data: %{status: %{execution: :error}}}, [:app_report_status]} =
               Data.apply_operation(data, operation)
    end

    test "changes status to :interrupted when evaluation fails with interrupt error" do
      data =
        data_after_operations!(Data.new(mode: :app), [
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1"]),
          {:queue_cells_evaluation, @cid, ["c2"]}
        ])

      operation =
        {:add_cell_evaluation_response, @cid, "c2", @eval_resp, eval_meta(interrupted: true)}

      assert {:ok, %{app_data: %{status: %{execution: :interrupted}}}, [:app_report_status]} =
               Data.apply_operation(data, operation)
    end

    test "changes status back to :executed after recovery" do
      data =
        data_after_operations!(Data.new(mode: :app), [
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2"]),
          {:reflect_main_evaluation_failure, @cid},
          evaluate_cells_operations(["setup", "c1"]),
          {:queue_cells_evaluation, @cid, ["c2"]}
        ])

      operation = {:add_cell_evaluation_response, @cid, "c2", @eval_resp, eval_meta()}

      assert {:ok, %{app_data: %{status: %{execution: :executed}}}, [:app_report_status]} =
               Data.apply_operation(data, operation)
    end

    test "when the app is shutting down and the last client leaves, returns terminate action" do
      data =
        data_after_operations!(Data.new(mode: :app), [
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:client_join, @cid, User.new()},
          {:app_shutdown, @cid}
        ])

      operation = {:client_leave, @cid}

      assert {:ok, %{app_data: %{status: %{lifecycle: :shutting_down}}}, [:app_terminate]} =
               Data.apply_operation(data, operation)
    end

    test "does not automatically reevaluate" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!(Data.new(mode: :app), [
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_cell_attributes, @cid, "c2", %{reevaluate_automatically: true}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()},
          evaluate_cells_operations(["c2"], bind_inputs: %{"c2" => ["i1"]})
        ])

      operation = {:set_input_value, @cid, "i1", "stuff"}

      assert {:ok, %{app_data: %{status: %{execution: :executed}}}, _actions} =
               Data.apply_operation(data, operation)
    end
  end

  describe "bound_cells_with_section/2" do
    test "returns an empty list when an invalid input id is given" do
      data = Data.new()
      assert [] = Data.bound_cells_with_section(data, "nonexistent")
    end

    test "returns code cells bound to the given input" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:insert_cell, @cid, "s1", 4, :code, "c4", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()},
          evaluate_cells_operations(["c2", "c3", "c4"], %{
            bind_inputs: %{"c2" => ["i1"], "c4" => ["i1"]}
          })
        ])

      assert [{%{id: "c2"}, _}, {%{id: "c4"}, _}] = Data.bound_cells_with_section(data, "i1")
    end
  end

  describe "cell_ids_for_full_evaluation/2" do
    test "includes changed cells with dependent ones" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:insert_cell, @cid, "s1", 3, :code, "c4", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3", "c4"],
            uses: %{"c4" => ["c2"]}
          ),
          # Modify cell 2
          {:client_join, @cid, User.new()},
          {:apply_cell_delta, @cid, "c2", :primary, Delta.new() |> Delta.insert("cats"), 1}
        ])

      assert Data.cell_ids_for_full_evaluation(data, []) |> Enum.sort() == ["c2", "c4"]
    end

    test "includes fresh cells" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c3", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c3"]),
          # Insert a fresh cell between cell 1 and cell 3
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}}
        ])

      assert Data.cell_ids_for_full_evaluation(data, []) |> Enum.sort() == ["c2"]
    end

    test "includes stale cells" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2"],
            uses: %{"c2" => ["c1"]}
          ),
          # Reevaluate cell 1
          evaluate_cells_operations(["c1"], versions: %{"c1" => 1})
        ])

      assert Data.cell_ids_for_full_evaluation(data, []) |> Enum.sort() == ["c2"]
    end

    test "includes forced cells with dependent ones" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:insert_cell, @cid, "s1", 3, :code, "c4", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c3", "c4"],
            uses: %{"c4" => ["c2"]}
          )
        ])

      assert Data.cell_ids_for_full_evaluation(data, ["c2"]) |> Enum.sort() == ["c2", "c4"]
    end

    test "excludes evaluating and queued cells" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          {:queue_cells_evaluation, @cid, ["c1", "c2"]}
        ])

      assert Data.cell_ids_for_full_evaluation(data, []) |> Enum.sort() == ["c3"]
    end
  end

  describe "cell_ids_for_reevaluation/2" do
    test "does not include the setup cell" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"])
        ])

      assert Data.cell_ids_for_reevaluation(data) == []
    end

    test "includes evaluated cells" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2"])
        ])

      assert Data.cell_ids_for_reevaluation(data) |> Enum.sort() == ["c1", "c2"]
    end

    test "includes stale cells" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2"],
            uses: %{"c2" => ["c1"]}
          ),
          # Reevaluate cell 1
          evaluate_cells_operations(["c1"], versions: %{"c1" => 1})
        ])

      assert Data.cell_ids_for_reevaluation(data) |> Enum.sort() == ["c1", "c2"]
    end

    test "stops reevaluation on the first fresh cell" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2"]),
          # Insert a new cell between the two evaluated cells
          {:insert_cell, @cid, "s1", 1, :code, "c3", %{}}
        ])

      assert Data.cell_ids_for_reevaluation(data) |> Enum.sort() == ["c1"]
    end

    test "considers each branch separately" do
      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_section, @cid, 1, "s2"},
          {:insert_cell, @cid, "s2", 0, :code, "c2", %{}},
          {:insert_cell, @cid, "s2", 1, :code, "c3", %{}},
          {:insert_section, @cid, 2, "s3"},
          {:insert_cell, @cid, "s3", 0, :code, "c4", %{}},
          {:set_section_parent, @cid, "s2", "s1"},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup", "c1", "c2", "c4"])
        ])

      assert Data.cell_ids_for_reevaluation(data) |> Enum.sort() == ["c1", "c2", "c4"]
    end
  end

  describe "changed_input_ids/1" do
    test "returns empty set when there are no inputs" do
      assert Data.changed_input_ids(Data.new()) == MapSet.new()
    end

    test "returns inputs which value changed since they have been bound to some cell" do
      input1 = %{id: "i1", type: :text, label: "Text", default: "hey"}
      input2 = %{id: "i2", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:insert_cell, @cid, "s1", 4, :code, "c4", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1", "c2"]},
          {:add_cell_evaluation_response, @cid, "c1", {:input, input1}, eval_meta()},
          {:add_cell_evaluation_response, @cid, "c2", {:input, input2}, eval_meta()},
          evaluate_cells_operations(["c3", "c4"], %{
            bind_inputs: %{"c3" => ["i1"], "c4" => ["i2"]}
          }),
          {:set_input_value, @cid, "i1", "new value"}
        ])

      assert Data.changed_input_ids(data) == MapSet.new(["i1"])
    end

    test "includes an input where one cell is bound with the old value and one with latest" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:insert_cell, @cid, "s1", 2, :code, "c3", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()},
          evaluate_cells_operations(["c2", "c3"], %{
            bind_inputs: %{"c2" => ["i1"], "c3" => ["i1"]}
          }),
          {:set_input_value, @cid, "i1", "new value"},
          # Reevaluate only cell 2, cell 3 is still stale
          evaluate_cells_operations(["c2"], %{bind_inputs: %{"c2" => ["i1"]}})
        ])

      assert Data.changed_input_ids(data) == MapSet.new(["i1"])
    end

    test "does not return removed inputs" do
      input = %{id: "i1", type: :text, label: "Text", default: "hey"}

      data =
        data_after_operations!([
          {:insert_section, @cid, 0, "s1"},
          {:insert_cell, @cid, "s1", 0, :code, "c1", %{}},
          {:insert_cell, @cid, "s1", 1, :code, "c2", %{}},
          {:set_runtime, @cid, connected_noop_runtime()},
          evaluate_cells_operations(["setup"]),
          {:queue_cells_evaluation, @cid, ["c1"]},
          {:add_cell_evaluation_response, @cid, "c1", {:input, input}, eval_meta()},
          evaluate_cells_operations(["c2"], %{bind_inputs: %{"c2" => ["i1"]}}),
          {:set_input_value, @cid, "i1", "new value"},
          {:delete_cell, @cid, "c1"}
        ])

      assert Data.changed_input_ids(data) == MapSet.new([])
    end
  end

  defp evaluate_cells_operations(cell_ids, opts \\ []) do
    uses = opts[:uses] || %{}
    versions = opts[:versions] || %{}
    bind_inputs = opts[:bind_inputs] || %{}

    [
      {:queue_cells_evaluation, @cid, cell_ids},
      for cell_id <- cell_ids do
        # For convenience we make each cell evaluation define an identifier
        # corresponding to the cell id, this way it is easy to make any
        # other cell depend on it
        metadata =
          eval_meta(
            defines: %{cell_id => versions[cell_id] || 0},
            uses: uses[cell_id] || []
          )

        [
          for input_id <- bind_inputs[cell_id] || [] do
            {:bind_input, @cid, cell_id, input_id}
          end,
          {:add_cell_evaluation_response, @cid, cell_id, @eval_resp, metadata}
        ]
      end
    ]
  end

  defp connected_noop_runtime() do
    {:ok, runtime} = Livebook.Runtime.NoopRuntime.new() |> Livebook.Runtime.connect()
    runtime
  end
end

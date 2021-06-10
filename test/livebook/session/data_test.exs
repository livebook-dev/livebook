defmodule Livebook.Session.DataTest do
  use ExUnit.Case, async: true

  alias Livebook.Session.Data
  alias Livebook.{Delta, Notebook}
  alias Livebook.Notebook.Cell
  alias Livebook.Users.User

  alias Livebook.Runtime.NoopRuntime

  describe "new/1" do
    test "called with no arguments defaults to a blank notebook" do
      empty_map = %{}

      assert %{notebook: %{sections: []}, cell_infos: ^empty_map, section_infos: ^empty_map} =
               Data.new()
    end

    test "called with a notebook, sets default cell and section infos" do
      cell = Notebook.Cell.new(:elixir)
      section = %{Notebook.Section.new() | cells: [cell]}
      notebook = %{Notebook.new() | sections: [section]}

      cell_id = cell.id
      section_id = section.id

      assert %{cell_infos: %{^cell_id => %{}}, section_infos: %{^section_id => %{}}} =
               Data.new(notebook)
    end
  end

  describe "apply_operation/2 given :insert_section" do
    test "adds new section to notebook and section info" do
      data = Data.new()

      operation = {:insert_section, self(), 0, "s1"}

      assert {:ok,
              %{
                notebook: %{
                  sections: [%{id: "s1"}]
                },
                section_infos: %{"s1" => _}
              }, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :insert_cell" do
    test "returns an error given invalid section id" do
      data = Data.new()
      operation = {:insert_cell, self(), "nonexistent", 0, :elixir, "c1"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "insert_cell adds new cell to notebook and cell info" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"}
        ])

      operation = {:insert_cell, self(), "s1", 0, :elixir, "c1"}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{cells: [%Cell.Elixir{id: "c1"}]}
                  ]
                },
                cell_infos: %{"c1" => _}
              }, []} = Data.apply_operation(data, operation)
    end

    test "initializes client-revision map" do
      client_pid = self()

      data =
        data_after_operations!([
          {:client_join, client_pid, User.new()},
          {:insert_section, self(), 0, "s1"}
        ])

      operation = {:insert_cell, self(), "s1", 0, :elixir, "c1"}

      assert {:ok,
              %{
                cell_infos: %{"c1" => %{revision_by_client_pid: %{^client_pid => 0}}}
              }, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :delete_section" do
    test "returns an error given invalid section id" do
      data = Data.new()
      operation = {:delete_section, self(), "nonexistent"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "removes the section from notebook and section info, adds to deleted sections" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"}
        ])

      operation = {:delete_section, self(), "s1"}
      empty_map = %{}

      assert {:ok,
              %{
                notebook: %{
                  sections: []
                },
                section_infos: ^empty_map,
                deleted_sections: [%{id: "s1"}]
              }, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :delete_cell" do
    test "returns an error given invalid cell id" do
      data = Data.new()
      operation = {:delete_cell, self(), "nonexistent"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "if the cell is evaluating, cencels section evaluation" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:queue_cell_evaluation, self(), "c2"}
        ])

      operation = {:delete_cell, self(), "c1"}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{evaluation_status: :ready}},
                section_infos: %{"s1" => %{evaluating_cell_id: nil, evaluation_queue: []}}
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "removes the cell from notebook and section info, adds to deleted cells" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"}
        ])

      operation = {:delete_cell, self(), "c1"}
      empty_map = %{}

      assert {:ok,
              %{
                notebook: %{
                  sections: [%{cells: []}]
                },
                cell_infos: ^empty_map,
                deleted_cells: [%{id: "c1"}]
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "unqueues the cell if it's queued for evaluation" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:queue_cell_evaluation, self(), "c2"}
        ])

      operation = {:delete_cell, self(), "c2"}

      assert {:ok,
              %{
                section_infos: %{"s1" => %{evaluation_queue: []}}
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "marks evaluated child cells as stale" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          # Evaluate both cells
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, [1, 2, 3]}},
          {:queue_cell_evaluation, self(), "c2"},
          {:add_cell_evaluation_response, self(), "c2", {:ok, [1, 2, 3]}}
        ])

      operation = {:delete_cell, self(), "c1"}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{validity_status: :stale}}
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "returns forget evaluation action" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"}
        ])

      operation = {:delete_cell, self(), "c1"}

      assert {:ok, _data, [{:forget_evaluation, %{id: "c1"}, %{id: "s1"}}]} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :move_cell" do
    test "returns an error given invalid cell id" do
      data = Data.new()
      operation = {:move_cell, self(), "nonexistent", 1}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given no offset" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"}
        ])

      operation = {:move_cell, self(), "c1", 0}
      assert :error = Data.apply_operation(data, operation)
    end

    test "given negative offset moves the cell and marks relevant cells as stale" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          # Add cells
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:insert_cell, self(), "s1", 2, :elixir, "c3"},
          {:insert_cell, self(), "s1", 3, :elixir, "c4"},
          # Evaluate cells
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c2"},
          {:add_cell_evaluation_response, self(), "c2", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c3"},
          {:add_cell_evaluation_response, self(), "c3", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c4"},
          {:add_cell_evaluation_response, self(), "c4", {:ok, nil}}
        ])

      operation = {:move_cell, self(), "c3", -1}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{
                      cells: [%{id: "c1"}, %{id: "c3"}, %{id: "c2"}, %{id: "c4"}]
                    }
                  ]
                },
                cell_infos: %{
                  "c1" => %{validity_status: :evaluated},
                  "c2" => %{validity_status: :stale},
                  "c3" => %{validity_status: :stale},
                  "c4" => %{validity_status: :stale}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "given positive offset moves the cell and marks relevant cells as stale" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          # Add cells
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:insert_cell, self(), "s1", 2, :elixir, "c3"},
          {:insert_cell, self(), "s1", 3, :elixir, "c4"},
          # Evaluate cells
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c2"},
          {:add_cell_evaluation_response, self(), "c2", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c3"},
          {:add_cell_evaluation_response, self(), "c3", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c4"},
          {:add_cell_evaluation_response, self(), "c4", {:ok, nil}}
        ])

      operation = {:move_cell, self(), "c2", 1}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{
                      cells: [%{id: "c1"}, %{id: "c3"}, %{id: "c2"}, %{id: "c4"}]
                    }
                  ]
                },
                cell_infos: %{
                  "c1" => %{validity_status: :evaluated},
                  "c2" => %{validity_status: :stale},
                  "c3" => %{validity_status: :stale},
                  "c4" => %{validity_status: :stale}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "allows moving cells between sections" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_section, self(), 1, "s2"},
          # Add cells
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:insert_cell, self(), "s2", 0, :elixir, "c3"},
          {:insert_cell, self(), "s2", 1, :elixir, "c4"}
        ])

      operation = {:move_cell, self(), "c2", 1}

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

    test "marks relevant cells in further sections as stale" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:insert_section, self(), 1, "s2"},
          {:insert_cell, self(), "s2", 0, :elixir, "c3"},
          # Evaluate cells
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c2"},
          {:add_cell_evaluation_response, self(), "c2", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c3"},
          {:add_cell_evaluation_response, self(), "c3", {:ok, nil}}
        ])

      operation = {:move_cell, self(), "c1", 1}

      assert {:ok,
              %{
                cell_infos: %{"c3" => %{validity_status: :stale}}
              }, []} = Data.apply_operation(data, operation)
    end

    test "moving a markdown cell does not change validity" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          # Add cells
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :markdown, "c2"},
          # Evaluate the Elixir cell
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, nil}}
        ])

      operation = {:move_cell, self(), "c2", -1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{validity_status: :evaluated}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "affected queued cell is unqueued" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          # Add cells
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          # Evaluate the Elixir cell
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:queue_cell_evaluation, self(), "c2"}
        ])

      operation = {:move_cell, self(), "c2", -1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{evaluation_status: :ready}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "does not invalidate the moved cell if the order of Elixir cells stays the same" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          # Add cells
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :markdown, "c2"},
          {:insert_cell, self(), "s1", 2, :elixir, "c3"},
          # Evaluate cells
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c3"},
          {:add_cell_evaluation_response, self(), "c3", {:ok, nil}}
        ])

      operation = {:move_cell, self(), "c1", 1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{validity_status: :evaluated},
                  "c3" => %{validity_status: :evaluated}
                }
              }, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :move_section" do
    test "returns an error given invalid section id" do
      data = Data.new()
      operation = {:move_section, self(), "nonexistent", 1}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given no offset" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_section, self(), 1, "s2"}
        ])

      operation = {:move_section, self(), "s2", 0}
      assert :error = Data.apply_operation(data, operation)
    end

    test "given negative offset moves the section and marks relevant cells as stale" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_section, self(), 1, "s2"},
          # Add cells
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:insert_cell, self(), "s2", 0, :elixir, "c3"},
          {:insert_cell, self(), "s2", 1, :elixir, "c4"},
          # Evaluate cells
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c2"},
          {:add_cell_evaluation_response, self(), "c2", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c3"},
          {:add_cell_evaluation_response, self(), "c3", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c4"},
          {:add_cell_evaluation_response, self(), "c4", {:ok, nil}}
        ])

      operation = {:move_section, self(), "s2", -1}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{
                      cells: [%{id: "c3"}, %{id: "c4"}]
                    },
                    %{
                      cells: [%{id: "c1"}, %{id: "c2"}]
                    }
                  ]
                },
                cell_infos: %{
                  "c1" => %{validity_status: :stale},
                  "c2" => %{validity_status: :stale},
                  "c3" => %{validity_status: :stale},
                  "c4" => %{validity_status: :stale}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "given positive offset moves the section and marks relevant cells as stale" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_section, self(), 1, "s2"},
          # Add cells
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:insert_cell, self(), "s2", 0, :elixir, "c3"},
          {:insert_cell, self(), "s2", 1, :elixir, "c4"},
          # Evaluate cells
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c2"},
          {:add_cell_evaluation_response, self(), "c2", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c3"},
          {:add_cell_evaluation_response, self(), "c3", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c4"},
          {:add_cell_evaluation_response, self(), "c4", {:ok, nil}}
        ])

      operation = {:move_section, self(), "s1", 1}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{
                      cells: [%{id: "c3"}, %{id: "c4"}]
                    },
                    %{
                      cells: [%{id: "c1"}, %{id: "c2"}]
                    }
                  ]
                },
                cell_infos: %{
                  "c1" => %{validity_status: :stale},
                  "c2" => %{validity_status: :stale},
                  "c3" => %{validity_status: :stale},
                  "c4" => %{validity_status: :stale}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "marks relevant cells in further sections as stale" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_section, self(), 1, "s2"},
          {:insert_section, self(), 2, "s3"},
          # Add cells
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s2", 1, :elixir, "c2"},
          {:insert_cell, self(), "s3", 0, :elixir, "c3"},
          # Evaluate cells
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c2"},
          {:add_cell_evaluation_response, self(), "c2", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c3"},
          {:add_cell_evaluation_response, self(), "c3", {:ok, nil}}
        ])

      operation = {:move_section, self(), "s1", 1}

      assert {:ok,
              %{
                cell_infos: %{"c3" => %{validity_status: :stale}}
              }, []} = Data.apply_operation(data, operation)
    end

    test "moving a section with only markdown cells does not change validity" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_section, self(), 1, "s2"},
          # Add cells
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s2", 0, :markdown, "c2"},
          # Evaluate the Elixir cell
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, nil}}
        ])

      operation = {:move_section, self(), "s2", -1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{validity_status: :evaluated}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "affected queued cells are unqueued" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_section, self(), 1, "s2"},
          # Add cells
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s2", 0, :elixir, "c2"},
          # Evaluate the Elixir cell
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:queue_cell_evaluation, self(), "c2"}
        ])

      operation = {:move_section, self(), "s2", -1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{evaluation_status: :ready}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "does not invalidate cells in moved section if the order of Elixir cells stays the same" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_section, self(), 1, "s2"},
          {:insert_section, self(), 2, "s3"},
          {:insert_section, self(), 3, "s4"},
          # Add cells
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s2", 0, :markdown, "c2"},
          {:insert_cell, self(), "s3", 0, :elixir, "c3"},
          {:insert_cell, self(), "s4", 0, :markdown, "c4"},
          # Evaluate cells
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, nil}},
          {:queue_cell_evaluation, self(), "c3"},
          {:add_cell_evaluation_response, self(), "c3", {:ok, nil}}
        ])

      operation = {:move_section, self(), "s4", -1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{validity_status: :evaluated},
                  "c3" => %{validity_status: :evaluated}
                }
              }, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :queue_cell_evaluation" do
    test "returns an error given invalid cell id" do
      data = Data.new()
      operation = {:queue_cell_evaluation, self(), "nonexistent"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given non-elixir cell" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :markdown, "c1"}
        ])

      operation = {:queue_cell_evaluation, self(), "c1"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error for an evaluating cell" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"}
        ])

      operation = {:queue_cell_evaluation, self(), "c1"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns start runtime action if there is no runtime and this is the first evaluation" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"}
        ])

      operation = {:queue_cell_evaluation, self(), "c1"}

      assert {:ok,
              %{
                cell_infos: %{"c1" => %{evaluation_status: :queued}},
                section_infos: %{"s1" => %{evaluating_cell_id: nil, evaluation_queue: ["c1"]}}
              }, [:start_runtime]} = Data.apply_operation(data, operation)
    end

    test "only queues the cell if runtime start has already been requested" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:queue_cell_evaluation, self(), "c1"}
        ])

      operation = {:queue_cell_evaluation, self(), "c2"}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{evaluation_status: :queued},
                  "c2" => %{evaluation_status: :queued}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil, evaluation_queue: ["c1", "c2"]}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "marks the cell as evaluating if the corresponding section is idle" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:set_runtime, self(), NoopRuntime.new()}
        ])

      operation = {:queue_cell_evaluation, self(), "c1"}

      assert {:ok,
              %{
                cell_infos: %{"c1" => %{evaluation_status: :evaluating}},
                section_infos: %{"s1" => %{evaluating_cell_id: "c1", evaluation_queue: []}}
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "returns start evaluation action if the corresponding section is idle" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:set_runtime, self(), NoopRuntime.new()}
        ])

      operation = {:queue_cell_evaluation, self(), "c1"}

      assert {:ok, _data, [{:start_evaluation, %{id: "c1"}, %{id: "s1"}}]} =
               Data.apply_operation(data, operation)
    end

    test "marks the cell as queued if the corresponding section is already evaluating" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"}
        ])

      operation = {:queue_cell_evaluation, self(), "c2"}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{evaluation_status: :queued}},
                section_infos: %{"s1" => %{evaluating_cell_id: "c1", evaluation_queue: ["c2"]}}
              }, []} = Data.apply_operation(data, operation)
    end

    test "marks the cell as queued if a previous section is already evaluating" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_section, self(), 1, "s2"},
          {:insert_cell, self(), "s2", 0, :elixir, "c2"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"}
        ])

      operation = {:queue_cell_evaluation, self(), "c2"}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{evaluation_status: :queued}},
                section_infos: %{
                  "s1" => %{evaluating_cell_id: "c1", evaluation_queue: []},
                  "s2" => %{evaluating_cell_id: nil, evaluation_queue: ["c2"]}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "queues previous unevaluated and stale cells" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:insert_section, self(), 1, "s2"},
          {:insert_cell, self(), "s2", 0, :elixir, "c3"},
          {:insert_cell, self(), "s2", 1, :elixir, "c4"},
          # Evaluate first 2 cells
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, [1, 2, 3]}},
          {:queue_cell_evaluation, self(), "c2"},
          {:add_cell_evaluation_response, self(), "c2", {:ok, [1, 2, 3]}},
          # Evaluate the first cell, so the second becomes stale
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, [1, 2, 3]}}
        ])

      # The above leads to:
      #
      # Section 1:
      # * cell 1 - evaluated
      # * cell 2 - stale
      # Section 2:
      # * cell 3 - fresh
      # * cell 4 - fresh
      #
      # Queuing cell 4 should also queue cell 3 and cell 2,
      # so that they all become evaluated.

      operation = {:queue_cell_evaluation, self(), "c4"}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{evaluation_status: :evaluating},
                  "c3" => %{evaluation_status: :queued},
                  "c4" => %{evaluation_status: :queued}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: "c2", evaluation_queue: []},
                  "s2" => %{evaluating_cell_id: nil, evaluation_queue: ["c3", "c4"]}
                }
              }, _actions} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :evaluation_started" do
    test "updates cell evaluation digest" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"}
        ])

      operation = {:evaluation_started, self(), "c1", "digest"}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{
                    evaluation_digest: "digest"
                  }
                }
              }, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :add_cell_evaluation_output" do
    test "updates the cell outputs" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"}
        ])

      operation = {:add_cell_evaluation_output, self(), "c1", "Hello!"}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{
                      cells: [%{outputs: ["Hello!"]}]
                    }
                  ]
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "merges consecutive stdout results" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_output, self(), "c1", "Hola"}
        ])

      operation = {:add_cell_evaluation_output, self(), "c1", " amigo!"}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{
                      cells: [%{outputs: ["Hola amigo!"]}]
                    }
                  ]
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "normalizes consecutive stdout results to respect CR" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_output, self(), "c1", "Hola"}
        ])

      operation = {:add_cell_evaluation_output, self(), "c1", "\ramigo!\r"}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{
                      cells: [%{outputs: ["amigo!\r"]}]
                    }
                  ]
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "adds output even after the cell finished evaluation" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, [1, 2, 3]}}
        ])

      operation = {:add_cell_evaluation_output, self(), "c1", "Hello!"}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{
                      cells: [%{outputs: ["Hello!", _result]}]
                    }
                  ]
                }
              }, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :add_cell_evaluation_response" do
    test "updates the cell outputs" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"}
        ])

      operation = {:add_cell_evaluation_response, self(), "c1", {:ok, [1, 2, 3]}}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{
                      cells: [%{outputs: [{:ok, [1, 2, 3]}]}]
                    }
                  ]
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "marks the cell as evaluated" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"}
        ])

      operation = {:add_cell_evaluation_response, self(), "c1", {:ok, [1, 2, 3]}}

      assert {:ok,
              %{
                cell_infos: %{"c1" => %{validity_status: :evaluated, evaluation_status: :ready}},
                section_infos: %{"s1" => %{evaluating_cell_id: nil, evaluation_queue: []}}
              }, []} = Data.apply_operation(data, operation)
    end

    test "marks next queued cell in this section as evaluating if there is one" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:queue_cell_evaluation, self(), "c2"}
        ])

      operation = {:add_cell_evaluation_response, self(), "c1", {:ok, [1, 2, 3]}}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{evaluation_status: :evaluating}},
                section_infos: %{"s1" => %{evaluating_cell_id: "c2", evaluation_queue: []}}
              },
              [{:start_evaluation, %{id: "c2"}, %{id: "s1"}}]} =
               Data.apply_operation(data, operation)
    end

    test "marks next queued cell in a further section as evaluating if there is one" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_section, self(), 1, "s2"},
          {:insert_cell, self(), "s2", 0, :elixir, "c2"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:queue_cell_evaluation, self(), "c2"}
        ])

      operation = {:add_cell_evaluation_response, self(), "c1", {:ok, [1, 2, 3]}}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{evaluation_status: :evaluating}},
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil, evaluation_queue: []},
                  "s2" => %{evaluating_cell_id: "c2", evaluation_queue: []}
                }
              },
              [{:start_evaluation, %{id: "c2"}, %{id: "s2"}}]} =
               Data.apply_operation(data, operation)
    end

    test "if parent cells are not evaluated, marks them for evaluation first" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:set_runtime, self(), NoopRuntime.new()}
        ])

      operation = {:queue_cell_evaluation, self(), "c2"}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{evaluation_status: :evaluating},
                  "c2" => %{evaluation_status: :queued}
                },
                section_infos: %{"s1" => %{evaluating_cell_id: "c1", evaluation_queue: ["c2"]}}
              },
              [{:start_evaluation, %{id: "c1"}, %{id: "s1"}}]} =
               Data.apply_operation(data, operation)
    end

    test "marks evaluated child cells as stale" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:insert_section, self(), 1, "s2"},
          {:insert_cell, self(), "s2", 0, :elixir, "c3"},
          # Evaluate all cells
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, [1, 2, 3]}},
          {:queue_cell_evaluation, self(), "c2"},
          {:add_cell_evaluation_response, self(), "c2", {:ok, [1, 2, 3]}},
          {:queue_cell_evaluation, self(), "c3"},
          {:add_cell_evaluation_response, self(), "c3", {:ok, [1, 2, 3]}},
          # Queue the first cell again
          {:queue_cell_evaluation, self(), "c1"}
        ])

      operation = {:add_cell_evaluation_response, self(), "c1", {:ok, [1, 2, 3]}}

      assert {:ok,
              %{
                cell_infos: %{
                  "c2" => %{validity_status: :stale},
                  "c3" => %{validity_status: :stale}
                }
              }, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :reflect_evaluation_failure" do
    test "clears evaluation queue and marks evaluated and evaluating cells as aborted" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:insert_cell, self(), "s1", 2, :elixir, "c3"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:queue_cell_evaluation, self(), "c2"},
          {:queue_cell_evaluation, self(), "c3"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, [1, 2, 3]}}
        ])

      operation = {:reflect_evaluation_failure, self()}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{validity_status: :aborted, evaluation_status: :ready},
                  "c2" => %{validity_status: :aborted, evaluation_status: :ready},
                  "c3" => %{validity_status: :fresh, evaluation_status: :ready}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil, evaluation_queue: []}
                }
              }, _actions} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :cancel_cell_evaluation" do
    test "returns an error given invalid cell id" do
      data = Data.new()
      operation = {:cancel_cell_evaluation, self(), "nonexistent"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error for an evaluated cell" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, [1, 2, 3]}}
        ])

      operation = {:cancel_cell_evaluation, self(), "c1"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "if the cell is evaluating, clears all sections evaluation and queues" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:insert_section, self(), 1, "s2"},
          {:insert_cell, self(), "s2", 0, :elixir, "c3"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:add_cell_evaluation_response, self(), "c1", {:ok, [1, 2, 3]}},
          {:queue_cell_evaluation, self(), "c2"},
          {:queue_cell_evaluation, self(), "c3"}
        ])

      operation = {:cancel_cell_evaluation, self(), "c2"}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{validity_status: :aborted, evaluation_status: :ready},
                  "c2" => %{validity_status: :aborted, evaluation_status: :ready},
                  "c3" => %{validity_status: :fresh, evaluation_status: :ready}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: nil, evaluation_queue: []},
                  "s2" => %{evaluating_cell_id: nil, evaluation_queue: []}
                }
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "if the cell is evaluating, returns stop evaluation action" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:queue_cell_evaluation, self(), "c2"}
        ])

      operation = {:cancel_cell_evaluation, self(), "c1"}

      assert {:ok, _data, [{:stop_evaluation, %{id: "s1"}}]} =
               Data.apply_operation(data, operation)
    end

    test "if the cell is queued, unqueues it" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:queue_cell_evaluation, self(), "c2"}
        ])

      operation = {:cancel_cell_evaluation, self(), "c2"}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{validity_status: :fresh, evaluation_status: :ready}},
                section_infos: %{"s1" => %{evaluating_cell_id: "c1", evaluation_queue: []}}
              }, []} = Data.apply_operation(data, operation)
    end

    test "if the cell is queued, unqueues dependent cells that are also queued" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:insert_cell, self(), "s1", 2, :elixir, "c3"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:queue_cell_evaluation, self(), "c2"},
          {:queue_cell_evaluation, self(), "c3"}
        ])

      operation = {:cancel_cell_evaluation, self(), "c2"}

      assert {:ok,
              %{
                cell_infos: %{"c3" => %{evaluation_status: :ready}},
                section_infos: %{"s1" => %{evaluating_cell_id: "c1", evaluation_queue: []}}
              }, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :set_notebook_name" do
    test "updates notebook name with the given string" do
      data = Data.new()

      operation = {:set_notebook_name, self(), "Cat's guide to life"}

      assert {:ok, %{notebook: %{name: "Cat's guide to life"}}, []} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :set_section_name" do
    test "returns an error given invalid cell id" do
      data = Data.new()
      operation = {:set_section_name, self(), "nonexistent", "Chapter 1"}
      assert :error = Data.apply_operation(data, operation)
    end

    test "updates section name with the given string" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"}
        ])

      operation = {:set_section_name, self(), "s1", "Cat's guide to life"}

      assert {:ok, %{notebook: %{sections: [%{name: "Cat's guide to life"}]}}, []} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :client_join" do
    test "returns an error if the given process is already a client" do
      user = User.new()

      data =
        data_after_operations!([
          {:client_join, self(), user}
        ])

      operation = {:client_join, self(), user}
      assert :error = Data.apply_operation(data, operation)
    end

    test "adds the given process and user to their corresponding maps" do
      client_pid = self()
      %{id: user_id} = user = User.new()
      data = Data.new()

      operation = {:client_join, client_pid, user}

      assert {:ok, %{clients_map: %{^client_pid => ^user_id}, users_map: %{^user_id => ^user}},
              []} = Data.apply_operation(data, operation)
    end

    test "adds new entry to the cell revisions map for the client with the latest revision" do
      client1_pid = IEx.Helpers.pid(0, 0, 0)
      user = User.new()
      delta1 = Delta.new() |> Delta.insert("cats")

      data =
        data_after_operations!([
          {:client_join, client1_pid, user},
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:apply_cell_delta, client1_pid, "c1", delta1, 1}
        ])

      client2_pid = IEx.Helpers.pid(0, 0, 1)
      operation = {:client_join, client2_pid, user}

      assert {:ok,
              %{
                cell_infos: %{"c1" => %{revision_by_client_pid: %{^client2_pid => 1}}}
              }, _} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :client_leave" do
    test "returns an error if the given process is not a client" do
      data = Data.new()

      operation = {:client_leave, self()}
      assert :error = Data.apply_operation(data, operation)
    end

    test "removes the given process from the client map" do
      data =
        data_after_operations!([
          {:client_join, self(), User.new()}
        ])

      operation = {:client_leave, self()}

      empty_map = %{}
      assert {:ok, %{clients_map: ^empty_map}, []} = Data.apply_operation(data, operation)
    end

    test "removes the corresponding user from users map if it has no more client processes" do
      data =
        data_after_operations!([
          {:client_join, self(), User.new()}
        ])

      operation = {:client_leave, self()}

      empty_map = %{}
      assert {:ok, %{users_map: ^empty_map}, []} = Data.apply_operation(data, operation)
    end

    test "leaves the corresponding user in users map if it has more client processes" do
      %{id: user_id} = user = User.new()
      client1_pid = IEx.Helpers.pid(0, 0, 0)
      client2_pid = IEx.Helpers.pid(0, 0, 1)

      data =
        data_after_operations!([
          {:client_join, client1_pid, user},
          {:client_join, client2_pid, user}
        ])

      operation = {:client_leave, client2_pid}

      assert {:ok, %{users_map: %{^user_id => ^user}}, []} = Data.apply_operation(data, operation)
    end

    test "removes an entry in the the cell revisions map for the client and purges deltas" do
      client1_pid = IEx.Helpers.pid(0, 0, 0)
      client2_pid = IEx.Helpers.pid(0, 0, 1)

      delta1 = Delta.new() |> Delta.insert("cats")

      data =
        data_after_operations!([
          {:client_join, client1_pid, User.new()},
          {:client_join, client2_pid, User.new()},
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:apply_cell_delta, client1_pid, "c1", delta1, 1}
        ])

      operation = {:client_leave, client2_pid}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{deltas: [], revision_by_client_pid: revision_by_client_pid}
                }
              }, _} = Data.apply_operation(data, operation)

      assert revision_by_client_pid == %{client1_pid => 1}
    end
  end

  describe "apply_operation/2 given :update_user" do
    test "returns an error if the given user is not a client" do
      data = Data.new()

      operation = {:update_user, self(), User.new()}
      assert :error = Data.apply_operation(data, operation)
    end

    test "updates users map" do
      %{id: user_id} = user = User.new()

      data =
        data_after_operations!([
          {:client_join, self(), user}
        ])

      updated_user = %{user | name: "Jake Peralta"}
      operation = {:update_user, self(), updated_user}

      assert {:ok, %{users_map: %{^user_id => ^updated_user}}, []} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :apply_cell_delta" do
    test "returns an error given invalid cell id" do
      data =
        data_after_operations!([
          {:client_join, self(), User.new()}
        ])

      operation = {:apply_cell_delta, self(), "nonexistent", Delta.new(), 1}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given non-joined client pid" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"}
        ])

      delta = Delta.new() |> Delta.insert("cats")
      operation = {:apply_cell_delta, self(), "c1", delta, 1}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given invalid revision" do
      data =
        data_after_operations!([
          {:client_join, self(), User.new()},
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"}
        ])

      delta = Delta.new() |> Delta.insert("cats")
      operation = {:apply_cell_delta, self(), "c1", delta, 5}

      assert :error = Data.apply_operation(data, operation)
    end

    test "updates cell source according to the given delta" do
      data =
        data_after_operations!([
          {:client_join, self(), User.new()},
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"}
        ])

      delta = Delta.new() |> Delta.insert("cats")
      operation = {:apply_cell_delta, self(), "c1", delta, 1}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{cells: [%{source: "cats"}]}
                  ]
                },
                cell_infos: %{"c1" => %{revision: 1}}
              }, _actions} = Data.apply_operation(data, operation)
    end

    test "transforms the delta if the revision is not the most recent" do
      client1_pid = IEx.Helpers.pid(0, 0, 0)
      client2_pid = IEx.Helpers.pid(0, 0, 1)

      delta1 = Delta.new() |> Delta.insert("cats")

      data =
        data_after_operations!([
          {:client_join, client1_pid, User.new()},
          {:client_join, client2_pid, User.new()},
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:apply_cell_delta, client1_pid, "c1", delta1, 1}
        ])

      delta2 = Delta.new() |> Delta.insert("tea")
      operation = {:apply_cell_delta, client2_pid, "c1", delta2, 1}

      assert {:ok,
              %{
                notebook: %{
                  sections: [
                    %{cells: [%{source: "catstea"}]}
                  ]
                },
                cell_infos: %{"c1" => %{revision: 2}}
              }, _} = Data.apply_operation(data, operation)
    end

    test "returns broadcast delta action with the transformed delta" do
      client1_pid = IEx.Helpers.pid(0, 0, 0)
      client2_pid = IEx.Helpers.pid(0, 0, 1)

      delta1 = Delta.new() |> Delta.insert("cats")

      data =
        data_after_operations!([
          {:client_join, client1_pid, User.new()},
          {:client_join, client2_pid, User.new()},
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:apply_cell_delta, client1_pid, "c1", delta1, 1}
        ])

      delta2 = Delta.new() |> Delta.insert("tea")
      operation = {:apply_cell_delta, client2_pid, "c1", delta2, 1}

      transformed_delta2 = Delta.new() |> Delta.retain(4) |> Delta.insert("tea")

      assert {:ok, _data, [{:broadcast_delta, ^client2_pid, _cell, ^transformed_delta2}]} =
               Data.apply_operation(data, operation)
    end

    test "given single client, does not keep deltas" do
      client_pid = self()

      data =
        data_after_operations!([
          {:client_join, client_pid, User.new()},
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"}
        ])

      delta = Delta.new() |> Delta.insert("cats")
      operation = {:apply_cell_delta, client_pid, "c1", delta, 1}

      assert {:ok,
              %{
                cell_infos: %{"c1" => %{deltas: []}}
              }, _} = Data.apply_operation(data, operation)
    end

    test "given multiple client, keeps the delta" do
      client1_pid = IEx.Helpers.pid(0, 0, 0)
      client2_pid = IEx.Helpers.pid(0, 0, 1)

      data =
        data_after_operations!([
          {:client_join, client1_pid, User.new()},
          {:client_join, client2_pid, User.new()},
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"}
        ])

      delta = Delta.new() |> Delta.insert("cats")
      operation = {:apply_cell_delta, client1_pid, "c1", delta, 1}

      assert {:ok,
              %{
                cell_infos: %{"c1" => %{deltas: [^delta]}}
              }, _} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :report_cell_revision" do
    test "returns an error given invalid cell id" do
      data =
        data_after_operations!([
          {:client_join, self(), User.new()}
        ])

      operation = {:report_cell_revision, self(), "nonexistent", 1}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given non-joined client pid" do
      client1_pid = IEx.Helpers.pid(0, 0, 0)
      client2_pid = IEx.Helpers.pid(0, 0, 1)

      data =
        data_after_operations!([
          {:client_join, client1_pid, User.new()},
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:apply_cell_delta, client1_pid, "c1", Delta.new(insert: "cats"), 1}
        ])

      operation = {:report_cell_revision, client2_pid, "c1", 1}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given invalid revision" do
      data =
        data_after_operations!([
          {:client_join, self(), User.new()},
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"}
        ])

      operation = {:report_cell_revision, self(), "c1", 1}
      assert :error = Data.apply_operation(data, operation)
    end

    test "updates client entry in the revisions map and purges unnecessary deltas" do
      client1_pid = IEx.Helpers.pid(0, 0, 0)
      client2_pid = IEx.Helpers.pid(0, 0, 1)

      delta1 = Delta.new() |> Delta.insert("cats")

      data =
        data_after_operations!([
          {:client_join, client1_pid, User.new()},
          {:client_join, client2_pid, User.new()},
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:apply_cell_delta, client1_pid, "c1", delta1, 1}
        ])

      operation = {:report_cell_revision, client2_pid, "c1", 1}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{
                    deltas: [],
                    revision_by_client_pid: %{^client1_pid => 1, ^client2_pid => 1}
                  }
                }
              }, _} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :set_cell_attributes" do
    test "returns an error given invalid cell id" do
      data = Data.new()

      operation = {:set_cell_attributes, self(), "nonexistent", %{}}
      assert :error = Data.apply_operation(data, operation)
    end

    test "returns an error given an unknown attribute key" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"}
        ])

      attrs = %{unknown: :value}
      operation = {:set_cell_attributes, self(), "c1", attrs}

      assert :error = Data.apply_operation(data, operation)
    end

    test "updates cell with the given attributes" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"}
        ])

      metadata = %{"disable_formatting" => true}
      attrs = %{metadata: metadata}
      operation = {:set_cell_attributes, self(), "c1", attrs}

      assert {:ok,
              %{
                notebook: %{
                  sections: [%{cells: [%{metadata: ^metadata}]}]
                }
              }, _} = Data.apply_operation(data, operation)
    end

    test "given input value change, marks evaluated child cells as stale" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :input, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c2"},
          {:add_cell_evaluation_response, self(), "c2", {:ok, [1, 2, 3]}}
        ])

      attrs = %{value: "stuff"}
      operation = {:set_cell_attributes, self(), "c1", attrs}

      assert {:ok,
              %{
                cell_infos: %{"c2" => %{validity_status: :stale}}
              }, _} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :set_runtime" do
    test "updates data with the given runtime" do
      data = Data.new()

      runtime = NoopRuntime.new()
      operation = {:set_runtime, self(), runtime}

      assert {:ok, %{runtime: ^runtime}, []} = Data.apply_operation(data, operation)
    end

    test "clears all statuses and the per-section queues" do
      data =
        data_after_operations!([
          # First section with evaluating and queued cells
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:insert_cell, self(), "s1", 1, :elixir, "c2"},
          {:set_runtime, self(), NoopRuntime.new()},
          {:queue_cell_evaluation, self(), "c1"},
          {:queue_cell_evaluation, self(), "c2"},
          # Second section with evaluating and queued cells
          {:insert_section, self(), 1, "s2"},
          {:insert_cell, self(), "s2", 0, :elixir, "c3"},
          {:insert_cell, self(), "s2", 1, :elixir, "c4"},
          {:queue_cell_evaluation, self(), "c3"},
          {:queue_cell_evaluation, self(), "c4"}
        ])

      runtime = NoopRuntime.new()
      operation = {:set_runtime, self(), runtime}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{validity_status: :aborted, evaluation_status: :ready},
                  "c2" => %{validity_status: :fresh, evaluation_status: :ready},
                  "c3" => %{validity_status: :fresh, evaluation_status: :ready},
                  "c4" => %{validity_status: :fresh, evaluation_status: :ready}
                },
                section_infos: %{
                  "s2" => %{evaluating_cell_id: nil, evaluation_queue: []},
                  "s1" => %{evaluating_cell_id: nil, evaluation_queue: []}
                }
              }, []} = Data.apply_operation(data, operation)
    end

    test "starts evaluation if there was no runtime before and there is now" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"},
          {:insert_cell, self(), "s1", 0, :elixir, "c1"},
          {:queue_cell_evaluation, self(), "c1"}
        ])

      runtime = NoopRuntime.new()
      operation = {:set_runtime, self(), runtime}

      assert {:ok,
              %{
                cell_infos: %{
                  "c1" => %{evaluation_status: :evaluating}
                },
                section_infos: %{
                  "s1" => %{evaluating_cell_id: "c1", evaluation_queue: []}
                }
              },
              [{:start_evaluation, %{id: "c1"}, %{id: "s1"}}]} =
               Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :set_path" do
    test "updates data with the given path" do
      data = Data.new()
      operation = {:set_path, self(), "path"}

      assert {:ok, %{path: "path"}, []} = Data.apply_operation(data, operation)
    end
  end

  describe "apply_operation/2 given :mark_as_not_dirty" do
    test "sets dirty flag to false" do
      data =
        data_after_operations!([
          {:insert_section, self(), 0, "s1"}
        ])

      operation = {:mark_as_not_dirty, self()}

      assert {:ok, %{dirty: false}, []} = Data.apply_operation(data, operation)
    end
  end

  defp data_after_operations!(operations) do
    Enum.reduce(operations, Data.new(), fn operation, data ->
      case Data.apply_operation(data, operation) do
        {:ok, data, _action} ->
          data

        :error ->
          raise "failed to set up test data, operation #{inspect(operation)} returned an error"
      end
    end)
  end
end

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
      cell1 = %{Cell.new(:markdown) | id: "1"}
      cell2 = %{Cell.new(:markdown) | id: "2"}
      cell3 = %{Cell.new(:markdown) | id: "3"}
      cell4 = %{Cell.new(:markdown) | id: "4"}

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
      cell1 = %{Cell.new(:markdown) | id: "1"}
      cell2 = %{Cell.new(:markdown) | id: "2"}

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
      cell1 = %{Cell.new(:markdown) | id: "1"}

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
end

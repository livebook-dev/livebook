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

  describe "input_cell_for_prompt/3" do
    test "returns an error if no input matches the given prompt" do
      cell1 = Cell.new(:elixir)

      notebook = %{
        Notebook.new()
        | sections: [
            %{Section.new() | cells: [cell1]}
          ]
      }

      assert :error = Notebook.input_cell_for_prompt(notebook, cell1.id, "name")
    end

    test "returns an input field if one is matching" do
      cell1 = %{Cell.new(:input) | name: "name", value: "Jake Peralta"}
      cell2 = Cell.new(:elixir)

      notebook = %{
        Notebook.new()
        | sections: [
            %{Section.new() | cells: [cell1, cell2]}
          ]
      }

      assert {:ok, ^cell1} = Notebook.input_cell_for_prompt(notebook, cell2.id, "name")
    end

    test "returns the first input if there are many with the same name" do
      cell1 = %{Cell.new(:input) | name: "name", value: "Amy Santiago"}
      cell2 = %{Cell.new(:input) | name: "name", value: "Jake Peralta"}
      cell3 = Cell.new(:elixir)

      notebook = %{
        Notebook.new()
        | sections: [
            %{Section.new() | cells: [cell1, cell2, cell3]}
          ]
      }

      assert {:ok, ^cell2} = Notebook.input_cell_for_prompt(notebook, cell3.id, "name")
    end

    test "returns longest-prefix input if many match the prompt" do
      cell1 = %{Cell.new(:input) | name: "name", value: "Amy Santiago"}
      cell2 = %{Cell.new(:input) | name: "nam", value: "Jake Peralta"}
      cell3 = Cell.new(:elixir)

      notebook = %{
        Notebook.new()
        | sections: [
            %{Section.new() | cells: [cell1, cell2, cell3]}
          ]
      }

      assert {:ok, ^cell1} = Notebook.input_cell_for_prompt(notebook, cell3.id, "name: ")
    end
  end
end

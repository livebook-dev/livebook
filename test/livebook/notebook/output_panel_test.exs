defmodule Livebook.Notebook.OutputPanelTest do
  use ExUnit.Case, async: true

  alias Livebook.{Notebook, Utils}
  alias Livebook.Notebook.OutputPanel, as: OP

  setup do
    # test_panel has the following structure:
    # | item1 | item2 | item3 |
    # |         item4         |
    # | item5 | item6 | item7 |
    item1 = OP.new_item("item1")
    item2 = OP.new_item("item2")
    item3 = OP.new_item("item3")
    item4 = OP.new_item("item4")
    item5 = OP.new_item("item5")
    item6 = OP.new_item("item6")
    item7 = OP.new_item("item7")

    test_panel =
      OP.new()
      |> OP.move_item_to_new_row(item1)
      |> OP.move_item_to_new_row(item2)
      |> OP.move_item_to_new_row(item3)
      |> OP.move_item_to_new_row(item4)
      |> OP.move_item_to_new_row(item5)
      |> OP.move_item_to_new_row(item6)
      |> OP.move_item_to_new_row(item7)
      |> OP.move_item(item2, {0, 1})
      |> OP.move_item(item3, {0, 2})
      |> OP.move_item(item6, {2, 1})
      |> OP.move_item(item7, {2, 2})

    %{test_panel: test_panel}
  end

  describe "get_item_position/2" do
    test "returns {2, 0} when looking for first item at row 3", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item5")
      position = OP.get_item_position(panel, item)
      assert position == {2, 0}
    end

    test "returns nil when item can't be found", %{test_panel: panel} do
      item = OP.new_item("random")
      position = OP.get_item_position(panel, item)
      assert position == nil
    end
  end

  describe "remove_item/2" do
    test "remove item from multi item row", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item2")
      panel = OP.remove_item(panel, item)
      assert length(hd(panel.rows) |> Map.get(:items)) == 2
      assert OP.get_item_by_cell_id(panel, "item1") |> Map.get(:width) == 50
      assert OP.get_item_position(panel, item) == nil
    end

    test "remove item from single item row", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item4")
      panel = OP.remove_item(panel, item)
      assert length(panel.rows) == 2
      assert OP.get_item_position(panel, item) == nil
    end

    test "remove not existing item", %{test_panel: panel} do
      item = OP.new_item("not_existing")
      new_panel = OP.remove_item(panel, item)
      assert new_panel == panel
    end
  end

  describe "move_item/3" do
    test "move single row item above", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item4")
      panel = OP.move_item(panel, item, {0, 0})
      assert length(panel.rows) == 2
      assert OP.get_item_position(panel, item) == {0, 0}
      assert OP.get_item_by_cell_id(panel, "item4") |> Map.get(:width) == 25
    end

    test "move single row item under", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item4")
      panel = OP.move_item(panel, item, {2, 1})
      assert length(panel.rows) == 2
      assert OP.get_item_position(panel, item) == {1, 1}
      assert OP.get_item_by_cell_id(panel, "item4") |> Map.get(:width) == 25
    end

    test "move item in same row to left", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item2")
      left_item = OP.get_item_by_cell_id(panel, "item1")
      panel = OP.move_item(panel, item, {0, 0})
      assert OP.get_item_position(panel, item) == {0, 0}
      assert OP.get_item_position(panel, left_item) == {0, 1}
      assert OP.get_item_by_cell_id(panel, "item2") |> Map.get(:width) == 33
    end

    test "move item in same row to right", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item2")
      right_item = OP.get_item_by_cell_id(panel, "item3")
      panel = OP.move_item(panel, item, {0, 3})
      assert OP.get_item_position(panel, item) == {0, 2}
      assert OP.get_item_position(panel, right_item) == {0, 1}
      assert OP.get_item_by_cell_id(panel, "item2") |> Map.get(:width) == 33
    end

    test "move item from multi row to mutli row", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item2")
      item1 = OP.get_item_by_cell_id(panel, "item1")
      panel = OP.move_item(panel, item, {2, 2})
      assert OP.get_item_by_cell_id(panel, "item2") |> Map.get(:width) == 25
      assert OP.get_item_by_cell_id(panel, "item1") |> Map.get(:width) == 50
    end

    test "move to invalid position at same row", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item4")
      new_panel = OP.move_item(panel, item, {2, 8})
      assert new_panel == panel
    end

    test "move to invalid position at different row", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item4")
      new_panel = OP.move_item(panel, item, {8, 8})
      assert new_panel == panel
    end

    test "move item to same location", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item4")
      new_panel = OP.move_item(panel, item, {1, 0})
      assert new_panel == panel
    end
  end

  describe "move_item_to_new_row/3" do
    test "move single item row to first row", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item4")
      panel = OP.move_item_to_new_row(panel, item, 0)
      assert OP.get_item_position(panel, item) == {0, 0}
      assert length(panel.rows) == 3
    end

    test "move single item row to last row", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item4")
      panel = OP.move_item_to_new_row(panel, item)
      assert OP.get_item_position(panel, item) == {2, 0}
      assert length(panel.rows) == 3
    end

    test "move multi item row to first row", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item5")
      panel = OP.move_item_to_new_row(panel, item, 0)
      assert OP.get_item_by_cell_id(panel, "item5") |> Map.get(:width) == 100
      assert OP.get_item_by_cell_id(panel, "item6") |> Map.get(:width) == 50
      assert OP.get_item_position(panel, item) == {0, 0}
      assert length(panel.rows) == 4
    end

    test "move multi item row to third row", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item1")
      panel = OP.move_item_to_new_row(panel, item, 2)
      assert OP.get_item_by_cell_id(panel, "item1") |> Map.get(:width) == 100
      assert OP.get_item_by_cell_id(panel, "item2") |> Map.get(:width) == 50
      assert OP.get_item_position(panel, item) == {2, 0}
      assert length(panel.rows) == 4
    end

    test "move item to same row", %{test_panel: panel} do
      item = OP.get_item_by_cell_id(panel, "item4")
      new_panel = OP.move_item_to_new_row(panel, item, 1)
      assert new_panel == panel
    end
  end
end

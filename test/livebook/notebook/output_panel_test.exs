defmodule Livebook.Notebook.OutputPanelTest do
  use ExUnit.Case, async: true

  alias Livebook.{Notebook, Utils}
  alias Livebook.Notebook.OutputPanel, as: OP

  setup do
    item = OP.new_item(Utils.random_id())

    five_row_panel =
      OP.new()
      |> OP.move_item_to_new_row(OP.new_item(Utils.random_id()))
      |> OP.move_item_to_new_row(OP.new_item(Utils.random_id()))
      |> OP.move_item_to_new_row(item)
      |> OP.move_item_to_new_row(OP.new_item(Utils.random_id()))
      |> OP.move_item_to_new_row(OP.new_item(Utils.random_id()))

    %{panel: five_row_panel, item: item}
  end

  describe "get_item_position/2" do
    test "returns {2, 0} when looking for item at row 2", %{panel: panel, item: item} do
      position = OP.get_item_position(panel, item)
      assert position == {2, 0}
    end

    test "returns nil when item can't be found", %{panel: panel} do
      item = OP.new_item(Utils.random_id())
      position = OP.get_item_position(panel, item)
      assert position == nil
    end
  end

  describe "remove_item/2" do
    test "remove one item", %{panel: panel, item: item} do
      panel = OP.remove_item(panel, item)
      assert length(panel.rows) == 4
    end
  end

  describe "move_item/3" do
    test "move item to top", %{panel: panel, item: item} do
      panel = OP.move_item(panel, item, {0, 0})
      first_row = hd(panel.rows)
      assert length(first_row.items) == 2
    end
  end

  describe "move_item_to_new_row/3" do
    test "move item to top", %{panel: panel, item: item} do
      panel = OP.move_item_to_new_row(panel, item, 0)
      assert hd(panel.rows) == %{items: [item]}
    end

    test "move item to last row", %{panel: panel, item: item} do
      panel = OP.move_item_to_new_row(panel, item)
      assert List.last(panel.rows) == %{items: [item]}
    end
  end
end

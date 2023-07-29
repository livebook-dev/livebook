defmodule Livebook.Notebook.OutputPanel do
  @moduledoc false

  # Data structure representing Output Panel in a notebook.
  #
  # There is only one Output Panel for each notebook and it is
  # a view to reorganize cell output.

  defstruct [:rows]

  alias Livebook.Notebook.Cell
  import Livebook.Utils, only: [access_by_id: 2]

  @type t :: %__MODULE__{
          rows: list(row)
        }

  @type row :: %{items: list(item)}
  @type item :: %{cell_id: Cell.id(), width: 0..100}
  @type item_position :: {integer(), integer()}

  @doc """
  Returns an empty Output Panel.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      rows: []
    }
  end

  @doc """
  Creates a new item.
  """
  @spec new_item(Cell.id(), integer()) :: item()
  def new_item(cell_id, width \\ 100) do
    %{cell_id: cell_id, width: width}
  end

  @doc """
  Returns the `item` of the given cell_id.
  """
  @spec get_item_by_cell_id(t(), Cell.id()) :: item() | nil
  def get_item_by_cell_id(panel, cell_id) do
    get_in(panel, [
      Access.key(:rows),
      Access.all(),
      Access.key(:items),
      access_by_id(cell_id, :cell_id)
    ])
    |> Enum.reject(&is_nil/1)
    |> List.first()
  end

  @doc """
  Inserts a new row at the given index and moves the given item to it.
  0 is the first row, -1 is the last row.
  """
  @spec move_item_to_new_row(t(), item(), integer()) :: t()
  def move_item_to_new_row(panel, item, row_index \\ -1) do
    old_position = get_item_position(panel, item)
    {panel, row_removed?} = remove_item_at(panel, old_position)

    update_in(panel.rows, fn rows ->
      item = set_item_width(item, 100)
      row = %{items: [item]}
      List.insert_at(rows, update_row_index(row_index, old_position, row_removed?), row)
    end)
  end

  defp update_row_index(row_index, nil, _), do: row_index

  defp update_row_index(row_index, {old_row_index, _}, true) when row_index > old_row_index,
    do: row_index - 1

  defp update_row_index(row_index, _, _), do: row_index

  @doc """
  Moves the item to the given position and updates the width of
  the influenced items.
  If the item is already in the output panel, it gets removed.
  If the position is invalid, the panel isn't updated.
  """
  @spec move_item(t(), item(), item_position()) :: t()
  def move_item(panel, item, position) do
    if valid_position?(panel, position) do
      old_position = get_item_position(panel, item)
      {panel, row_removed?} = remove_item_at(panel, old_position)
      insert_item(panel, item, update_position(position, old_position, row_removed?))
    else
      panel
    end
  end

  defp update_position(position, nil, _), do: position

  defp update_position({same_row, col_index}, {same_row, old_col_index}, _)
       when col_index > old_col_index,
       do: {same_row, col_index - 1}

  defp update_position({row_index, col_index}, {old_row_index, _}, true)
       when row_index > old_row_index,
       do: {row_index - 1, col_index}

  defp update_position(position, _, _), do: position

  @doc """
  Removes the item from the output panel.
  If it's the only item in the row, the whole row is removed.
  Otherwise, the space left behind is spread out amoung the other items in the row.
  """
  @spec remove_item(t(), item()) :: t()
  def remove_item(panel, item) do
    position = get_item_position(panel, item)
    {panel, _} = remove_item_at(panel, position)
    panel
  end

  @doc """
  Returns the position of the given item as a tuple {row, column}.
  When not found it returns nil.
  Only returns the first occurance of the item. Since items are unique
  this shouldn't be a problem.
  """
  @spec get_item_position(t(), item()) :: item_position() | nil
  def get_item_position(panel, item) do
    find_position_in_rows(panel.rows, item[:cell_id], 0)
  end

  defp find_position_in_rows(_rows, nil, _row_index), do: nil
  defp find_position_in_rows([], _cell_id, _row_index), do: nil

  defp find_position_in_rows([row | rows], cell_id, row_index) do
    case find_position_in_items(row.items, cell_id, 0) do
      nil ->
        find_position_in_rows(rows, cell_id, row_index + 1)

      column_index ->
        {row_index, column_index}
    end
  end

  defp find_position_in_items([], _cell_id, _column_index), do: nil

  defp find_position_in_items([item | items], cell_id, column_index) do
    if item.cell_id == cell_id,
      do: column_index,
      else: find_position_in_items(items, cell_id, column_index + 1)
  end

  defp valid_position?(panel, {row_index, col_index}) do
    row = Enum.at(panel.rows, row_index)
    row && col_index <= length(row.items)
  end

  defp insert_item(panel, item, {row_index, col_index}) do
    update_in(panel, [Access.key(:rows), Access.at(row_index), Access.key(:items)], fn items ->
      num_items = length(items)
      width = div(100, num_items + 1)

      List.insert_at(items, col_index, item)
      |> Enum.map(fn item ->
        %{item | width: width}
      end)
    end)
  end

  defp remove_item_at(panel, nil), do: {panel, false}

  defp remove_item_at(panel, {row_index, col_index}) do
    row = Enum.at(panel.rows, row_index)
    num_items = length(row.items)

    panel =
      update_in(panel.rows, fn rows ->
        if num_items == 1 do
          List.delete_at(rows, row_index)
        else
          update_in(rows, [Access.at(row_index), Access.key(:items)], fn items ->
            {removed_item, updated_items} = List.pop_at(items, col_index)

            Enum.map(updated_items, fn item ->
              %{item | width: div(100, num_items - 1)}
            end)
          end)
        end
      end)

    {panel, num_items == 1}
  end

  defp set_item_width(item, width) do
    %{item | width: width}
  end
end

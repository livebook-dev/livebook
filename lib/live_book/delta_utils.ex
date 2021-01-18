defmodule LiveBook.DeltaUtils do
  def apply_delta_to_text(text, delta) do
    apply_operations_to_text(text, delta.ops)
  end

  defp apply_operations_to_text(text, []), do: text

  defp apply_operations_to_text(text, [%{retain: n} | ops]) do
    {left, right} = String.split_at(text, n)
    left <> apply_operations_to_text(right, ops)
  end

  defp apply_operations_to_text(text, [%{insert: inserted} | ops]) do
    inserted <> apply_operations_to_text(text, ops)
  end

  defp apply_operations_to_text(text, [%{delete: n} | ops]) do
    apply_operations_to_text(String.slice(text, n..-1), ops)
  end

  def delta_to_map(delta) do
    %{ops: delta.ops}
  end

  def delta_from_map(%{"ops" => ops}) do
    ops
    |> Enum.map(&parse_operation/1)
    |> TextDelta.new()
  end

  defp parse_operation(%{"insert" => insert}), do: %{insert: insert}
  defp parse_operation(%{"retain" => retain}), do: %{retain: retain}
  defp parse_operation(%{"delete" => delete}), do: %{delete: delete}
end

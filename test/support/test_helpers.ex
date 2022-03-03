defmodule Livebook.TestHelpers do
  @moduledoc false

  alias Livebook.Session.Data

  @doc """
  Creates file structure according to the given specification.
  """
  def create_tree!(path, items) do
    for {name, content} <- items do
      child_path = Path.join(path, to_string(name))

      case content do
        items when is_list(items) ->
          File.mkdir!(child_path)
          create_tree!(child_path, items)

        content when is_binary(content) ->
          File.write!(child_path, content)
      end
    end
  end

  @doc """
  Applies the given list of operations to `Livebook.Session.Data`.

  Raises if any of the operations results in an error.
  """
  def data_after_operations!(data \\ Data.new(), operations) do
    Enum.reduce(operations, data, fn operation, data ->
      case Data.apply_operation(data, operation) do
        {:ok, data, _action} ->
          data

        :error ->
          raise "failed to set up test data, operation #{inspect(operation)} returned an error"
      end
    end)
  end
end

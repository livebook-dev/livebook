defmodule Livebook.TestHelpers do
  @moduledoc false

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
end

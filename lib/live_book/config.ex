defmodule LiveBook.Config do
  @moduledoc false

  @doc """
  Checks if the distribution mode is configured to use short names.
  """
  @spec shortnames?() :: boolean()
  def shortnames?() do
    case Application.fetch_env!(:live_book, :node_type) do
      :shortnames -> true
      :longnames -> false
    end
  end
end

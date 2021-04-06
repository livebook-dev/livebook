defmodule Livebook.Config do
  @moduledoc false

  @doc """
  Checks if the distribution mode is configured to use short names.
  """
  @spec shortnames?() :: boolean()
  def shortnames?() do
    case Application.fetch_env!(:livebook, :node) do
      {:shortnames, _host} -> true
      {:longnames, _host} -> false
    end
  end
end

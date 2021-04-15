defmodule Livebook.Config do
  @moduledoc false

  @doc """
  Checks if the distribution mode is configured to use short names.
  """
  @spec shortnames?() :: boolean()
  def shortnames?() do
    case Application.get_env(:livebook, :node) do
      nil -> true
      {:shortnames, _name} -> true
      {:longnames, _name} -> false
    end
  end

  @doc """
  Return the root path for persisting notebooks.
  """
  @spec root_path() :: binary()
  def root_path() do
    Application.get_env(:livebook, :root_path, File.cwd!())
  end
end

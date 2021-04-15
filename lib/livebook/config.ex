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
  Returns the authentication mode.
  """
  @spec auth_mode() :: :token | :password | :disabled
  def auth_mode() do
    Application.fetch_env!(:livebook, :authentication_mode)
  end
end

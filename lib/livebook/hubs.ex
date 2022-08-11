defprotocol Livebook.Hubs do
  @moduledoc false

  alias Livebook.Hubs.Hub

  @doc """
  Gets a list of hubs.
  """
  @spec fetch_hubs(any()) :: {:ok, list(Hub.t())} | {:error, any()}
  def fetch_hubs(hub)
end

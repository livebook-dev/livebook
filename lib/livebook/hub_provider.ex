defprotocol Livebook.HubProvider do
  @moduledoc false

  @doc """
  Gets a list of hubs.
  """
  @spec fetch_hubs(any()) :: {:ok, list(Livebook.HubProvider.Hub.t())} | {:error, any()}
  def fetch_hubs(hub)
end

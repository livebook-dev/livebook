defprotocol Livebook.Hubs.HubProvider do
  @moduledoc false

  @doc """
  Gets a list of hubs.
  """
  @spec fetch_hubs(any()) :: {:ok, list(Livebook.Hubs.Hub.t())} | {:error, any()}
  def fetch_hubs(hub)
end

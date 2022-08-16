defprotocol Livebook.Hubs.HubProvider do
  @moduledoc false

  @doc """
  Normalize given data to `Hub` struct.
  """
  @spec to_hub(any()) :: Livebook.Hubs.Hub.t()
  def to_hub(data)
end

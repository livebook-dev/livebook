defmodule Livebook.Hubs.Local do
  @moduledoc false
  defstruct [:id, :hub_name, :hub_color]
end

defimpl Livebook.Hubs.Provider, for: Livebook.Hubs.Local do
  def load(%Livebook.Hubs.Local{} = local, fields) do
    %{local | id: fields.id, hub_name: fields.hub_name, hub_color: fields.hub_color}
  end

  def normalize(%Livebook.Hubs.Local{} = local) do
    %Livebook.Hubs.Metadata{
      id: local.id,
      name: local.hub_name,
      provider: local,
      color: local.hub_color
    }
  end

  def type(_), do: "local"
end

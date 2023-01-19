defmodule Livebook.Hubs.Local do
  @moduledoc false

  defstruct [:id, :hub_name, :hub_emoji]
end

defimpl Livebook.Hubs.Provider, for: Livebook.Hubs.Local do
  def load(local, fields) do
    %{local | id: fields.id, hub_name: fields.hub_name, hub_emoji: fields.hub_emoji}
  end

  def normalize(local) do
    %Livebook.Hubs.Metadata{
      id: local.id,
      name: local.hub_name,
      provider: local,
      emoji: local.hub_emoji
    }
  end

  def type(_local), do: "local"

  def connect(_local), do: nil

  def connected?(_local), do: false

  def disconnect(_local), do: :ok
end

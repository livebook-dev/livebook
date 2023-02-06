defmodule Livebook.Hubs.Local do
  @moduledoc false

  defstruct [:id, :hub_name, :hub_emoji]
end

defimpl Livebook.Hubs.Provider, for: Livebook.Hubs.Local do
  def load(local, fields) do
    %{local | id: fields.id, hub_name: fields.hub_name, hub_emoji: fields.hub_emoji}
  end

  def to_metadata(local) do
    %Livebook.Hubs.Metadata{
      id: local.id,
      name: local.hub_name,
      provider: local,
      emoji: local.hub_emoji,
      connected?: false
    }
  end

  def type(_local), do: "local"

  def connection_spec(_local), do: nil

  def disconnect(_local), do: :ok

  def capabilities(_local), do: []

  def get_secrets(_local), do: []

  def create_secret(_local, _secret), do: :ok

  def connection_error(_local), do: nil
end

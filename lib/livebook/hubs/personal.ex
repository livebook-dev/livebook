defmodule Livebook.Hubs.Personal do
  @moduledoc false

  defstruct [:id, :hub_name, :hub_emoji]
end

defimpl Livebook.Hubs.Provider, for: Livebook.Hubs.Personal do
  def load(personal, fields) do
    %{personal | id: fields.id, hub_name: fields.hub_name, hub_emoji: fields.hub_emoji}
  end

  def to_metadata(personal) do
    %Livebook.Hubs.Metadata{
      id: personal.id,
      name: personal.hub_name,
      provider: personal,
      emoji: personal.hub_emoji,
      connected?: false
    }
  end

  def type(_personal), do: "personal"

  def connection_spec(_personal), do: nil

  def disconnect(_personal), do: :ok

  def capabilities(_personal), do: []

  def get_secrets(_personal), do: []

  def create_secret(_personal, _secret), do: :ok

  def connection_error(_personal), do: nil
end

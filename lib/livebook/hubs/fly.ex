defmodule Livebook.Hubs.Fly do
  @moduledoc false
  defstruct [
    :id,
    :access_token,
    :hub_name,
    :hub_color,
    :organization_id,
    :organization_type,
    :organization_name,
    :application_id
  ]

  @type t :: %__MODULE__{
          id: Livebook.Utils.id(),
          access_token: String.t(),
          hub_name: String.t(),
          hub_color: Livebook.Users.User.hex_color(),
          organization_id: String.t(),
          organization_type: String.t(),
          organization_name: String.t(),
          application_id: String.t()
        }

  def save_fly(fly, params) do
    fly = %{fly | hub_name: params["hub_name"], hub_color: params["hub_color"]}

    Livebook.Hubs.save_hub(fly)
  end
end

defimpl Livebook.Hubs.Provider, for: Livebook.Hubs.Fly do
  def load(%Livebook.Hubs.Fly{} = fly, fields) do
    %{
      fly
      | id: fields.id,
        access_token: fields.access_token,
        hub_name: fields.hub_name,
        hub_color: fields.hub_color,
        organization_id: fields.organization_id,
        organization_type: fields.organization_type,
        organization_name: fields.organization_name,
        application_id: fields.application_id
    }
  end

  def normalize(%Livebook.Hubs.Fly{} = fly) do
    %Livebook.Hubs.Metadata{
      id: fly.id,
      name: fly.hub_name,
      provider: fly,
      color: fly.hub_color
    }
  end

  def type(_), do: "fly"
end

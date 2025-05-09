defmodule Livebook.Users.User do
  # Represents a Livebook user.
  #
  # Livebook users are not regular web app accounts,
  # but rather ephemeral data about the clients
  # using the app. Every person using Livebook
  # can provide data like name and cursor color
  # to improve visibility during collaboration.

  use Ecto.Schema
  import Ecto.Changeset

  alias Livebook.Utils

  @type access_type :: :full | :apps

  @type t :: %__MODULE__{
          id: id(),
          name: String.t() | nil,
          email: String.t() | nil,
          avatar_url: String.t() | nil,
          access_type: access_type(),
          groups: list(map()) | nil,
          payload: map() | nil,
          hex_color: hex_color()
        }

  @type id :: Utils.id()
  @type hex_color :: String.t()

  embedded_schema do
    field :name, :string
    field :email, :string
    field :avatar_url, :string
    field :access_type, Ecto.Enum, values: ~w[full apps]a, default: :full
    field :groups, {:array, :map}, default: []
    field :payload, :map
    field :hex_color, Livebook.EctoTypes.HexColor
  end

  @doc """
  Generates a new user.
  """
  @spec new(String.t()) :: t()
  def new(id \\ Utils.random_long_id()) do
    %__MODULE__{
      id: id,
      name: nil,
      email: nil,
      avatar_url: nil,
      access_type: :full,
      groups: [],
      payload: nil,
      hex_color: Livebook.EctoTypes.HexColor.random()
    }
  end

  @doc false
  def changeset(user, attrs \\ %{}) do
    user
    |> cast(attrs, [:name, :email, :avatar_url, :access_type, :groups, :hex_color, :payload])
    |> validate_required([:hex_color])
  end
end

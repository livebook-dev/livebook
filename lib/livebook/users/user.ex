defmodule Livebook.Users.User do
  @moduledoc false

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

  @type t :: %__MODULE__{
          id: id(),
          name: String.t() | nil,
          email: String.t() | nil,
          hex_color: hex_color()
        }

  @type id :: Utils.id()
  @type hex_color :: String.t()

  embedded_schema do
    field :name, :string
    field :email, :string
    field :hex_color, Livebook.EctoTypes.HexColor
  end

  @doc """
  Generates a new user.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      id: Utils.random_id(),
      name: nil,
      email: nil,
      hex_color: Livebook.EctoTypes.HexColor.random()
    }
  end

  def changeset(user, attrs \\ %{}) do
    user
    |> cast(attrs, [:id, :name, :email, :hex_color])
    |> validate_required([:id, :name, :hex_color])
  end
end

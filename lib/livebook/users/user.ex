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
          hex_color: hex_color()
        }

  @type id :: Utils.id()
  @type hex_color :: String.t()

  embedded_schema do
    field(:name, :string)
    field(:hex_color, :string)
  end

  @doc """
  Generates a new user.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      id: Utils.random_id(),
      name: nil,
      hex_color: random_hex_color()
    }
  end

  def changeset(user, attrs \\ %{}) do
    user
    |> cast(attrs, [:id, :name, :hex_color])
    |> validate_required([:id, :name, :hex_color])
    |> validate_color()
  end

  defp validate_color(changeset) do
    case get_field(changeset, :hex_color) do
      nil ->
        changeset

      hex_color ->
        if Utils.valid_hex_color?(hex_color) do
          changeset
        else
          add_error(changeset, :hex_color, "not a valid color")
        end
    end
  end

  @doc """
  Returns a random hex color for a user.

  ## Options

    * `:except` - a list of colors to omit
  """
  def random_hex_color(opts \\ []) do
    colors = [
      # red
      "#F87171",
      # yellow
      "#FBBF24",
      # green
      "#6EE7B7",
      # blue
      "#60A5FA",
      # purple
      "#A78BFA",
      # pink
      "#F472B6",
      # salmon
      "#FA8072",
      # mat green
      "#9ED9CC"
    ]

    except = opts[:except] || []
    colors = colors -- except

    Enum.random(colors)
  end
end

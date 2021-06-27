defmodule Livebook.Users.User do
  @moduledoc false

  # Represents a Livebook user.
  #
  # Livebook users are not regular web app accounts,
  # but rather ephemeral data about the clients
  # using the app. Every person using Livebook
  # can provide data like name and cursor color
  # to improve visibility during collaboration.

  defstruct [:id, :name, :hex_color]

  alias Livebook.Utils

  @type t :: %__MODULE__{
          id: id(),
          name: String.t() | nil,
          hex_color: hex_color()
        }

  @type id :: Utils.id()
  @type hex_color :: String.t()

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

  @doc """
  Validates `attrs` and returns an updated user.

  In case of validation errors `{:error, errors, user}` tuple
  is returned, where `user` is partially updated by using
  only the valid attributes.
  """
  @spec change(t(), %{binary() => any()}) :: {:ok, t()} | {:error, list(String.t()), t()}
  def change(user, attrs \\ %{}) do
    {user, []}
    |> change_name(attrs)
    |> change_hex_color(attrs)
    |> case do
      {user, []} -> {:ok, user}
      {user, errors} -> {:error, errors, user}
    end
  end

  defp change_name({user, errors}, %{"name" => ""}) do
    {%{user | name: nil}, errors}
  end

  defp change_name({user, errors}, %{"name" => name}) do
    {%{user | name: name}, errors}
  end

  defp change_name({user, errors}, _attrs), do: {user, errors}

  defp change_hex_color({user, errors}, %{"hex_color" => hex_color}) do
    if Utils.valid_hex_color?(hex_color) do
      {%{user | hex_color: hex_color}, errors}
    else
      {user, [{:hex_color, "not a valid color"} | errors]}
    end
  end

  defp change_hex_color({user, errors}, _attrs), do: {user, errors}

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

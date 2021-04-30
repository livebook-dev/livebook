defmodule Livebook.Users.User do
  defstruct [:id, :name, :color]

  alias Livebook.Utils

  @type id :: Utils.id()

  def new() do
    %__MODULE__{
      id: Utils.random_id(),
      name: nil,
      color: random_color()
    }
  end

  def change(user, attrs \\ %{}) do
    {user, []}
    |> change_name(attrs)
    |> change_color(attrs)
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

  defp change_color({user, errors}, %{"color" => color}) do
    if color_valid?(color) do
      {%{user | color: color}, errors}
    else
      {user, [{:color, "not a valid color"} | errors]}
    end
  end

  defp change_color({user, errors}, _attrs), do: {user, errors}

  defp color_valid?(color), do: color =~ ~r/^#[0-9a-fA-F]{6}$/

  def random_color(opts \\ []) do
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

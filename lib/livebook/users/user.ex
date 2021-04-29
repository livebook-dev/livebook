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

  def random_color() do
    # TODO: use HSV and vary H only? or predefined list of neat colors?
    #   - we want the color to fit white text, so just gather a reasonable list of colors
    # TODO: also color picker
    #   - native picker would trigger change too often, we need either a lib
    #     or something else
    Enum.random(["#F87171", "#FBBF24", "#6EE7B7", "#60A5FA", "#818CF8", "#A78BFA", "#F472B6"])
  end
end

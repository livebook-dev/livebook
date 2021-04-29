defmodule Livebook.Users.User do
  defstruct [:id, :name, :color]

  alias Livebook.Utils

  @type id :: Utils.id()

  def new(attrs \\ %{}) do
    %__MODULE__{
      id: Utils.random_id(),
      name: Map.get(attrs, :name, nil),
      color: Map.get_lazy(attrs, :color, &random_color/0)
    }
  end

  def color_valid?(color), do: color =~ ~r/^#[0-9a-fA-F]{6}$/

  def random_color() do
    # TODO: use HSV and vary H only? or predefined list of neat colors?
    #   - we want the color to fit white text, so just gather a reasonable list of colors
    # TODO: also color picker
    #   - native picker would trigger change too often, we need either a lib
    #     or something else
    Enum.random(["#F87171", "#FBBF24", "#6EE7B7", "#60A5FA", "#818CF8", "#A78BFA", "#F472B6"])
  end
end

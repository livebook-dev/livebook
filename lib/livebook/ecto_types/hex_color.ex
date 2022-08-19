defmodule Livebook.EctoTypes.HexColor do
  @moduledoc false
  use Ecto.Type

  alias Livebook.Utils

  def type, do: :string

  def cast(value), do: validate(value)
  def load(value), do: validate(value)
  def dump(value), do: validate(value)

  defp validate(value) do
    if Utils.valid_hex_color?(value) do
      {:ok, value}
    else
      {:error, message: "not a valid color"}
    end
  end
end

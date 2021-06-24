defmodule Livebook.Notebook.Cell.Input do
  @moduledoc false

  # A cell with an input field.
  #
  # It consists of an input that the user may fill
  # and then read during code evaluation.

  defstruct [:id, :metadata, :type, :name, :value, :reactive]

  alias Livebook.Utils
  alias Livebook.Notebook.Cell

  @type t :: %__MODULE__{
          id: Cell.id(),
          metadata: Cell.metadata(),
          type: type(),
          name: String.t(),
          value: String.t(),
          reactive: boolean()
        }

  @type type :: :text | :url | :number | :password

  @doc """
  Returns an empty cell.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      id: Utils.random_id(),
      metadata: %{},
      type: :text,
      name: "input",
      value: "",
      reactive: false
    }
  end

  @doc """
  Checks if the input cell contains a valid value
  for its type.
  """
  @spec validate(t()) :: :ok | {:error, String.t()}
  def validate(cell) do
    validate_value(cell.value, cell.type)
  end

  defp validate_value(_value, :text), do: :ok

  defp validate_value(_value, :password), do: :ok

  defp validate_value(value, :url) do
    if Utils.valid_url?(value) do
      :ok
    else
      {:error, "not a valid URL"}
    end
  end

  defp validate_value(value, :number) do
    case Float.parse(value) do
      {_number, ""} -> :ok
      _ -> {:error, "not a valid number"}
    end
  end

  @doc """
  Checks if the input changed in terms of content.
  """
  @spec invalidated?(t(), t()) :: boolean()
  def invalidated?(cell, prev_cell) do
    cell.value != prev_cell.value or cell.name != prev_cell.name
  end

  @doc """
  Checks if the input change should trigger reactive update.
  """
  @spec reactive_update?(t(), t()) :: boolean()
  def reactive_update?(cell, prev_cell) do
    cell.reactive and cell.value != prev_cell.value and validate(cell) == :ok
  end
end

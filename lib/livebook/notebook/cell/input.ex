defmodule Livebook.Notebook.Cell.Input do
  @moduledoc false

  # A cell with an input field.
  #
  # It consists of an input that the user may fill
  # and then read during code evaluation.

  defstruct [:id, :type, :name, :value, :reactive, :props]

  alias Livebook.Utils
  alias Livebook.Notebook.Cell

  @type t :: %__MODULE__{
          id: Cell.id(),
          type: type(),
          name: String.t(),
          value: String.t(),
          reactive: boolean(),
          props: props()
        }

  # Make sure to keep this in sync with `type_from_string/1`
  @type type ::
          :text | :url | :number | :password | :textarea | :color | :range | :select | :checkbox

  @typedoc """
  Additional properties adjusting the given input type.
  """
  @type props :: %{atom() => term()}

  @doc """
  Returns an empty cell.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      id: Utils.random_id(),
      type: :text,
      name: "input",
      value: "",
      reactive: false,
      props: %{}
    }
  end

  @doc """
  Checks if the input cell contains a valid value
  for its type.
  """
  @spec validate(t()) :: :ok | {:error, String.t()}
  def validate(cell)

  def validate(%{value: value, type: :url}) do
    if Utils.valid_url?(value) do
      :ok
    else
      {:error, "not a valid URL"}
    end
  end

  def validate(%{value: value, type: :number}) do
    case Float.parse(value) do
      {_number, ""} -> :ok
      _ -> {:error, "not a valid number"}
    end
  end

  def validate(%{value: value, type: :color}) do
    if Utils.valid_hex_color?(value) do
      :ok
    else
      {:error, "not a valid hex color"}
    end
  end

  def validate(%{value: value, type: :range, props: props}) do
    case Float.parse(value) do
      {number, ""} ->
        cond do
          number < props.min -> {:error, "number too small"}
          number > props.max -> {:error, "number too big"}
          true -> :ok
        end

      _ ->
        {:error, "not a valid number"}
    end
  end

  def validate(_cell), do: :ok

  @doc """
  Returns default properties for input of the given type.
  """
  @spec default_props(type()) :: props()
  def default_props(type)

  def default_props(:range), do: %{min: 0, max: 100, step: 1}
  def default_props(:select), do: %{options: [""]}
  def default_props(_type), do: %{}

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

  @doc """
  Parses input type from string.
  """
  @spec type_from_string(String.t()) :: {:ok, type()} | :error
  def type_from_string(string) do
    case string do
      "text" -> {:ok, :text}
      "url" -> {:ok, :url}
      "number" -> {:ok, :number}
      "password" -> {:ok, :password}
      "textarea" -> {:ok, :textarea}
      "color" -> {:ok, :color}
      "range" -> {:ok, :range}
      "select" -> {:ok, :select}
      "checkbox" -> {:ok, :checkbox}
      _other -> :error
    end
  end

  @doc """
  Converts inpu type to string.
  """
  @spec type_to_string(type()) :: String.t()
  def type_to_string(type) do
    Atom.to_string(type)
  end
end

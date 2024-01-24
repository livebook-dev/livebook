defmodule Livebook.Text.Selection do
  # A text selection holding one or more selection ranges.
  #
  # This is a minimal representation of an editor selection, specifically
  # for the purpose of serialization and transformation. It does not
  # perform any normalizations, such as deduplication and range merging,
  # since this is already handled on the client during deserialization.

  defstruct [:ranges]

  @type t :: %__MODULE__{ranges: list(range())}

  @type range :: {anchor :: non_neg_integer(), head :: non_neg_integer()}

  def new(ranges) do
    if ranges == [] do
      raise ArgumentError, "text selection must have at least a single range"
    end

    %__MODULE__{ranges: ranges}
  end

  @doc """
  Transforms `selection` against `delta`.

  Selections are ultimately a group of positions, so each position is
  transformed. See `Livebook.Text.Delta.transform_position/2` for more
  details.
  """
  @spec transform(t(), Livebook.Text.Delta.t()) :: t()
  def transform(selection, delta) do
    ranges =
      for {anchor, head} <- selection.ranges do
        {Livebook.Text.Delta.transform_position(delta, anchor),
         Livebook.Text.Delta.transform_position(delta, head)}
      end

    %__MODULE__{ranges: ranges}
  end

  @doc """
  Converts the given selection to a compact representation, suitable
  for JSON serialization.
  """
  @spec to_compressed(t()) :: list()
  def to_compressed(selection) do
    for {anchor, head} <- selection.ranges, do: [anchor, head]
  end

  @doc """
  Builds a new selection from the given compact representation.
  """
  @spec from_compressed(list()) :: t()
  def from_compressed(list) do
    ranges = for [anchor, head] <- list, do: {anchor, head}
    %__MODULE__{ranges: ranges}
  end
end

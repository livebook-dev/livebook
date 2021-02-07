defmodule LiveBook.Utils do
  @moduledoc false

  @type id :: binary()

  @doc """
  Generates a random binary id.
  """
  @spec random_id() :: binary()
  def random_id() do
    :crypto.strong_rand_bytes(20) |> Base.encode32(case: :lower)
  end

  @doc """
  Generates a random short binary id.
  """
  @spec random_short_id() :: binary()
  def random_short_id() do
    :crypto.strong_rand_bytes(5) |> Base.encode32(case: :lower)
  end
end

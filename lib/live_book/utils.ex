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
  Wraps the given expression so that it's executed
  with the given process as the group leader.

  Always restores the original group leader afterwards.
  """
  defmacro with_group_leader(group_leader, do: expression) do
    quote do
      original_gl = Process.group_leader()

      try do
        Process.group_leader(self(), unquote(group_leader))
        unquote(expression)
      after
        Process.group_leader(self(), original_gl)
      end
    end
  end
end

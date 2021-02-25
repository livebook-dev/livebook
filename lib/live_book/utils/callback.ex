defmodule LiveBook.Utils.Callback do
  @moduledoc false

  # A wrapper struct for arity 1 procedure.
  #
  # The structure is meant to be used via the `Collectable` protocol
  # as a way to call the given function for every item.

  defstruct [:fun]

  @type t :: %__MODULE__{fun: fun()}

  @type fun :: (term() -> term())

  @doc """
  Builds a new structure wrapping the given function.
  """
  @spec new(fun()) :: t()
  def new(fun), do: %__MODULE__{fun: fun}

  @doc """
  Simply calls the underlying function with the given argument.
  """
  @spec call(t(), term()) :: :ok
  def call(callback, arg) do
    callback.fun.(arg)
    :ok
  end
end

defimpl Collectable, for: LiveBook.Utils.Callback do
  alias LiveBook.Utils.Callback

  def into(original) do
    collector_fun = fn
      callback, {:cont, item} ->
        Callback.call(callback, item)
        callback

      callback, :done ->
        callback

      _collectable_fun, :halt ->
        :ok
    end

    {original, collector_fun}
  end
end

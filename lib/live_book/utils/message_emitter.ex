defmodule LiveBook.Utils.MessageEmitter do
  @moduledoc false

  # A wrapper struct for sending transformed messages to a specific process.
  #
  # The structure is meant to be used via the `Collectable` protocol
  # as a way to send messages to a specific process.

  defstruct [:terget_pid, :transform]

  @type t :: %__MODULE__{
                terget_pid: pid(),
                transform: transform()
              }

  @type transform :: (term() -> term())

  @doc """
  Builds a new structure where `target_pid` represents
  the destination process and `transform` is a function
  used to map emitted items into messages.
  """
  @spec new(pid(), transform()) :: t()
  def new(terget_pid, transform) do
    %__MODULE__{terget_pid: terget_pid, transform: transform}
  end

  @doc """
  Transforms the given item with emitter's `transform` function
  and sends the resulting message to the `target_pid`.

  Returns unchanged emitter.
  """
  @spec emit(t(), term()) :: t()
  def emit(emitter, item) do
    message = emitter.transform.(item)
    send(emitter.terget_pid, message)
    emitter
  end
end

defimpl Collectable, for: LiveBook.Utils.MessageEmitter do
  alias LiveBook.Utils.MessageEmitter

  def into(original) do
    collector_fun = fn
      emitter, {:cont, item} -> MessageEmitter.emit(emitter, item)
      emitter, :done -> emitter
      _emitter, :halt -> :ok
    end

    {original, collector_fun}
  end
end

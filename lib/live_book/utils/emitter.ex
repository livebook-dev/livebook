defmodule Livebook.Utils.Emitter do
  @moduledoc false

  # A wrapper struct for sending messages to the specified process.

  defstruct [:target_pid, :ref, :mapper]

  @type t :: %__MODULE__{
          target_pid: pid(),
          ref: reference(),
          mapper: mapper()
        }

  @type mapper :: (term() -> term())

  @doc """
  Builds a new structure where `target_pid` represents
  the process that will receive all emitted items.
  """
  @spec new(pid()) :: t()
  def new(target_pid) do
    %__MODULE__{target_pid: target_pid, ref: make_ref(), mapper: &Function.identity/1}
  end

  @doc """
  Sends {:emitter, ref, item} message to the `target_pid`.

  Note that item may be transformed with emitter's `mapper`
  if there is one, see `Emitter.mapper/2`.
  """
  @spec emit(t(), term()) :: :ok
  def emit(emitter, item) do
    message = {:emitter, emitter.ref, emitter.mapper.(item)}
    send(emitter.target_pid, message)
    :ok
  end

  @doc """
  Returns a new emitter that maps all emitted items with `mapper`.
  """
  @spec mapper(t(), mapper()) :: t()
  def mapper(emitter, mapper) do
    mapper = fn x -> mapper.(emitter.mapper.(x)) end
    %{emitter | mapper: mapper}
  end
end

defimpl Collectable, for: Livebook.Utils.Emitter do
  alias Livebook.Utils.Emitter

  def into(emitter) do
    collector_fun = fn
      :ok, {:cont, item} -> Emitter.emit(emitter, item)
      :ok, _ -> :ok
    end

    {:ok, collector_fun}
  end
end

defmodule Livebook.Runtime.NoopRuntime do
  @moduledoc false

  # A runtime that doesn't do any actual evaluation,
  # thus not requiring any underlying resources.

  defstruct []

  def new(), do: %__MODULE__{}

  defimpl Livebook.Runtime do
    def connect(_), do: :ok
    def disconnect(_), do: :ok
    def evaluate_code(_, _, _, _, _, _ \\ []), do: :ok
    def forget_evaluation(_, _, _), do: :ok
    def drop_container(_, _), do: :ok
    def request_completion_items(_, _, _, _, _, _), do: :ok
    def duplicate(_), do: {:ok, Livebook.Runtime.NoopRuntime.new()}
  end
end

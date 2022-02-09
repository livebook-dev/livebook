defmodule Livebook.Runtime.NoopRuntime do
  @moduledoc false

  # A runtime that doesn't do any actual evaluation,
  # thus not requiring any underlying resources.

  defstruct []

  def new(), do: %__MODULE__{}

  defimpl Livebook.Runtime do
    def connect(_, _), do: make_ref()
    def disconnect(_), do: :ok
    def evaluate_code(_, _, _, _, _ \\ []), do: :ok
    def forget_evaluation(_, _), do: :ok
    def drop_container(_, _), do: :ok
    def handle_intellisense(_, _, _, _, _), do: :ok
    def duplicate(_), do: {:ok, Livebook.Runtime.NoopRuntime.new()}
    def standalone?(_), do: false

    def read_file(_, path) do
      case File.read(path) do
        {:ok, content} -> {:ok, content}
        {:error, posix} -> {:error, posix |> :file.format_error() |> List.to_string()}
      end
    end
  end
end

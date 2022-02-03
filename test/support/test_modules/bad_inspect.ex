defmodule Livebook.TestModules.BadInspect do
  defstruct []

  defimpl Inspect do
    def inspect(%Livebook.TestModules.BadInspect{}, _opts) do
      :bad_return
    end
  end
end

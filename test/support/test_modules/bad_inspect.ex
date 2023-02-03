defmodule Livebook.TestModules.BadInspect do
  defstruct []

  defimpl Inspect do
    @dialyzer {:nowarn_function, inspect: 2}

    def inspect(%Livebook.TestModules.BadInspect{}, _opts) do
      :bad_return
    end
  end
end

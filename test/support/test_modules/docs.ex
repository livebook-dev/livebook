defmodule Livebook.TestModules.Docs.Without do
end

defmodule Livebook.TestModules.Docs.ModuleHidden do
  @moduledoc false
end

defmodule Livebook.TestModules.Docs.Module do
  @moduledoc "Hello."
end

defmodule Livebook.TestModules.Docs.FunctionHidden do
  @doc false
  def hello(), do: "hello"
end

defmodule Livebook.TestModules.Docs.Function do
  @doc "Hello."
  def hello(), do: "hello"
end

defmodule Livebook.Config.EnvBackend do
  @doc false

  defstruct []
end

defimpl Livebook.ConfigBackend, for: Livebook.Config.EnvBackend do
  def get(_backend, key), do: Application.get_env(:livebook, key)

  def put(backend, key, value) do
    Application.put_env(:livebook, key, value, persistent: true)

    backend
  end

  def load(backend), do: backend
end

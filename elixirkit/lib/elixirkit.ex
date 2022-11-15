defmodule ElixirKit do
  def publish(name, data) when is_atom(name) and is_binary(data) do
    ElixirKit.NIF.publish(name, data)
  end

  def subscribe do
    {:ok, _} = Registry.register(ElixirKit.Registry, "subscribers", [])
  end

  def bundle(release) do
    ElixirKit.Bundler.bundle_release(release)
  end
end

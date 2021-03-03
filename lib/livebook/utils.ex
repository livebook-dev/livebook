defmodule Livebook.Utils do
  @moduledoc false

  @type id :: binary()

  @doc """
  Generates a random binary id.
  """
  @spec random_id() :: binary()
  def random_id() do
    :crypto.strong_rand_bytes(20) |> Base.encode32(case: :lower)
  end

  @doc """
  Generates a random short binary id.
  """
  @spec random_short_id() :: binary()
  def random_short_id() do
    :crypto.strong_rand_bytes(5) |> Base.encode32(case: :lower)
  end

  @doc """
  Converts the given name to node identifier.
  """
  @spec node_from_name(String.t()) :: atom()
  def node_from_name(name) do
    if name =~ "@" do
      String.to_atom(name)
    else
      # Default to the same host as the current node
      [_, host] = node() |> Atom.to_string() |> String.split("@")
      :"#{name}@#{host}"
    end
  end

  @doc """
  Registers the given process under `name` for the time of `fun` evaluation.
  """
  @spec temporarily_register(pid(), atom(), (... -> any())) :: any()
  def temporarily_register(pid, name, fun) do
    Process.register(pid, name)
    fun.()
  after
    Process.unregister(name)
  end
end

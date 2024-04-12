defmodule Livebook.ZTA do
  @doc false
  def init do
    :ets.new(__MODULE__, [:named_table, :public, :set, read_concurrency: true])
  end

  def get(name) do
    :ets.lookup_element(__MODULE__, name, 2)
  end

  def put(name, value) do
    :ets.insert(__MODULE__, [{name, value}])
  end
end

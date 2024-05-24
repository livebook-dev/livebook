defmodule Livebook.Proxy.Handler do
  @moduledoc false

  def child_spec(opts) do
    name = Keyword.fetch!(opts, :name)
    listen = Keyword.fetch!(opts, :listen)
    :persistent_term.put({__MODULE__, name}, listen)
    PartitionSupervisor.child_spec(child_spec: Task.Supervisor, name: name)
  end

  def serve(parent, name, data) when is_pid(parent) and is_atom(name) do
    Process.link(parent)
    ref = Process.monitor(parent)
    conn = struct!(Plug.Conn, %{data | adapter: {Livebook.Proxy.Adapter, {parent, ref}}})
    :persistent_term.get({__MODULE__, name}).(conn)
  end

  def get_pid(name, key) do
    GenServer.whereis({:via, PartitionSupervisor, {name, key}})
  end
end

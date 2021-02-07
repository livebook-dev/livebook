defmodule LiveBook.Runtime.Standalone do
  defstruct [:node, :primary_pid]

  alias LiveBook.Utils

  @type t :: %__MODULE__{
          node: node(),
          primary_pid: pid()
        }

  def init() do
    id = Utils.random_id()
    node = :"live_book_runtime_#{id}@127.0.0.1"

    waiter = :"live_book_waiter_#{id}"
    Process.register(self(), waiter)

    eval = child_eval(waiter, node()) |> Macro.to_string()

    elixir_path = System.find_executable("elixir")

    Port.open({:spawn_executable, elixir_path}, [
      # Don't use stdio, so that the caller does not receive
      # unexpected messages if the process produces some output.
      :nouse_stdio,
      args: ["--name", to_string(node), "--eval", eval]
    ])

    # TODO: timeout or something with error?
    primary_pid =
      receive do
        {:node_started, ^node, primary_pid} ->
          Process.unregister(waiter)
          primary_pid
      end

    {:ok, %__MODULE__{node: node, primary_pid: primary_pid}}
  end

  defp child_eval(waiter, parent_node) do
    quote do
      parent_process = {unquote(waiter), unquote(parent_node)}
      ref = Process.monitor(parent_process)
      send(parent_process, {:node_started, node(), self()})

      receive do
        {:DOWN, ^ref, :process, _object, _reason} ->
          :ok

        :stop ->
          :ok
      end
    end
  end
end

defimpl LiveBook.Runtime, for: LiveBook.Runtime.Standalone do
  def get_node(runtime), do: runtime.node

  def disconnect(runtime) do
    send(runtime.primary_pid, :stop)
    :ok
  end
end

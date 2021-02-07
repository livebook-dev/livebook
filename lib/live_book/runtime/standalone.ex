defmodule LiveBook.Runtime.Standalone do
  defstruct [:node, :primary_pid]

  alias LiveBook.Utils

  @type t :: %__MODULE__{
          node: node(),
          primary_pid: pid()
        }

  def init(owner_pid) do
    case System.find_executable("elixir") do
      nil ->
        {:error, :no_elixir_executable}

      elixir_path ->
        id = Utils.random_short_id()
        node = :"live_book_runtime_#{id}@127.0.0.1"

        waiter = :"live_book_waiter_#{id}"
        Process.register(self(), waiter)

        eval = child_eval(waiter, node()) |> Macro.to_string()

        Port.open({:spawn_executable, elixir_path}, [
          # Don't use stdio, so that the caller does not receive
          # unexpected messages if the process produces some output.
          :nouse_stdio,
          args: ["--name", to_string(node), "--eval", eval]
        ])

        # TODO: timeout or something with error?
        receive do
          {:node_started, ^node, primary_pid} ->
            Process.unregister(waiter)

            send(primary_pid, {:set_owner, owner_pid})

            {:ok, %__MODULE__{node: node, primary_pid: primary_pid}}
        end
    end
  end

  defp child_eval(waiter, parent_node) do
    quote do
      defmodule LiveBook.Remote.Standalone.Client do
        def init(waiter) do
          ref = Process.monitor(waiter)

          loop(%{owner_ref: ref})
        end

        defp loop(state) do
          owner_ref = state.owner_ref

          receive do
            {:DOWN, ^owner_ref, :process, _object, _reason} ->
              :ok

            :stop ->
              :ok

            {:set_owner, pid} ->
              Process.demonitor(state.owner_ref)
              owner_ref = Process.monitor(pid)
              loop(%{state | owner_ref: owner_ref})
          end
        end
      end

      waiter_process = {unquote(waiter), unquote(parent_node)}
      send(waiter_process, {:node_started, node(), self()})
      LiveBook.Remote.Standalone.Client.init(waiter_process)
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

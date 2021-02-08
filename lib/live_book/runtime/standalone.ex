defmodule LiveBook.Runtime.Standalone do
  defstruct [:node, :primary_pid]

  # Represents an Elixir node managed by LiveBook.
  #
  # LiveBook is responsible for starting and terminating the node.
  # Most importantly we have to make sure the started node doesn't
  # stay in the system when the session or the entire LiveBook terminates.

  alias LiveBook.Utils

  @type t :: %__MODULE__{
          node: node(),
          primary_pid: pid()
        }

  @doc """
  Initializes the runtime by starting an Elixir node (i.e. a system process).

  The new node monitors the given owner process and terminates
  as soon as it terminates. It may also be terminated manually
  by using `Runtime.disconnect/1`.

  Note: to start the node it is required that `elixir` is a recognised
  executable within the system.
  """
  @spec init(pid()) :: {:ok, t()} | {:error, :no_elixir_executable | :timeout}
  def init(owner_pid) do
    case System.find_executable("elixir") do
      nil ->
        {:error, :no_elixir_executable}

      elixir_path ->
        id = Utils.random_short_id()

        node = :"live_book_runtime_#{id}@127.0.0.1"

        # The new Elixir node receives a code to evaluate
        # and we have to pass the current pid there, but since pid
        # is not a type we can include directly in the code,
        # we temporarily register the current process under a name.
        waiter = :"live_book_waiter_#{id}"
        Process.register(self(), waiter)

        eval = child_node_eval(waiter, node()) |> Macro.to_string()

        # Here we create a port to start the system process in a non-blocking way.
        Port.open({:spawn_executable, elixir_path}, [
          # Don't use stdio, so that the caller does not receive
          # unexpected messages if the process produces some output.
          :nouse_stdio,
          args: [
            "--name",
            to_string(node),
            "--eval",
            eval,
            # Minimize shedulers busy wait threshold,
            # so that they go to sleep immediately after evaluation.
            "--erl",
            "+sbwt none +sbwtdcpu none +sbwtdio none"
          ]
        ])

        receive do
          {:node_started, ^node, primary_pid} ->
            # Unregister the temporary name as it's no longer needed.
            Process.unregister(waiter)
            # Having the other process pid we can send the owner pid as a message.
            send(primary_pid, {:node_acknowledged, owner_pid})

            {:ok, %__MODULE__{node: node, primary_pid: primary_pid}}
        after
          10_000 ->
            {:error, :timeout}
        end
    end
  end

  defp child_node_eval(waiter, parent_node) do
    # This is the code that's gonna be evaluated in the newly
    # spawned Elixir runtime. This is the primary process
    # and as soon as it finishes, the runtime terminates.
    quote do
      # Initiate communication with the waiting process on the parent node.
      send({unquote(waiter), unquote(parent_node)}, {:node_started, node(), self()})

      receive do
        {:node_acknowledged, owner_pid} ->
          owner_ref = Process.monitor(owner_pid)

          # Wait until either the owner process terminates
          # or we receives an explicit stop request.
          receive do
            {:DOWN, ^owner_ref, :process, _object, _reason} ->
              :ok

            :stop ->
              :ok
          end
      after
        10_000 -> :timeout
      end
    end
  end
end

defimpl LiveBook.Runtime, for: LiveBook.Runtime.Standalone do
  def get_node(runtime), do: runtime.node

  def disconnect(runtime) do
    # Send a message to the main node process so it knows to terminate.
    send(runtime.primary_pid, :stop)

    :ok
  end
end

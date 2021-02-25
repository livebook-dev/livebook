defmodule LiveBook.Runtime.ElixirStandalone do
  defstruct [:node, :primary_pid, :init_ref]

  # A runtime backed by a standalone Elixir node managed by LiveBook.
  #
  # LiveBook is responsible for starting and terminating the node.
  # Most importantly we have to make sure the started node doesn't
  # stay in the system when the session or the entire LiveBook terminates.

  alias LiveBook.Utils
  require LiveBook.Utils
  import LiveBook.Runtime.StandaloneInit

  @type t :: %__MODULE__{
          node: node(),
          primary_pid: pid(),
          init_ref: reference()
        }

  @doc """
  Starts a new Elixir node (i.e. a system process) and initializes
  it with LiveBook-specific modules and processes.

  The new node monitors the given owner process and terminates
  as soon as it terminates. It may also be terminated manually
  by using `Runtime.disconnect/1`.

  Note: to start the node it is required that `elixir` is a recognised
  executable within the system.
  """
  @spec init(pid()) :: {:ok, t()} | {:error, String.t()}
  def init(owner_pid) do
    parent_node = node()
    {child_node, waiter} = init_parameteres()

    Utils.registered_as self(), waiter do
      with {:ok, elixir_path} <- find_elixir_executable(),
           eval <- child_node_ast(parent_node, waiter) |> Macro.to_string(),
           port <- start_elixir_node(elixir_path, child_node, eval),
           {:ok, primary_pid, init_ref} <- parent_init_sequence(child_node, port, owner_pid) do
        runtime = %__MODULE__{
          node: child_node,
          primary_pid: primary_pid,
          init_ref: init_ref
        }

        {:ok, runtime}
      else
        {:error, error} ->
          {:error, error}
      end
    end
  end

  defp start_elixir_node(elixir_path, node_name, eval) do
    # Here we create a port to start the system process in a non-blocking way.
    Port.open({:spawn_executable, elixir_path}, [
      :nouse_stdio,
      args: elixir_flags(node_name) ++ ["--eval", eval]
    ])
  end
end

defimpl LiveBook.Runtime, for: LiveBook.Runtime.ElixirStandalone do
  alias LiveBook.Runtime.ErlDist

  def connect(runtime) do
    ErlDist.Manager.set_owner(runtime.node, self())
    Process.monitor({ErlDist.Manager, runtime.node})
  end

  def disconnect(runtime) do
    ErlDist.Manager.stop(runtime.node)
    # Instruct the other node to terminate
    send(runtime.primary_pid, {:stop, runtime.init_ref})
  end

  def evaluate_code(runtime, code, container_ref, evaluation_ref, prev_evaluation_ref \\ :initial) do
    ErlDist.Manager.evaluate_code(
      runtime.node,
      code,
      container_ref,
      evaluation_ref,
      prev_evaluation_ref
    )
  end

  def forget_evaluation(runtime, container_ref, evaluation_ref) do
    ErlDist.Manager.forget_evaluation(runtime.node, container_ref, evaluation_ref)
  end

  def drop_container(runtime, container_ref) do
    ErlDist.Manager.drop_container(runtime.node, container_ref)
  end
end

defmodule LiveBook.Runtime.MixStandalone do
  defstruct [:node, :primary_pid, :init_ref, :project_path]

  # A runtime backed by a standalone Elixir node managed by LiveBook.
  #
  # This runtime is similar to `LiveBook.Runtime.ElixirStandalone`,
  # but the node is started in the context of a Mix project.

  alias LiveBook.Utils
  require LiveBook.Utils
  import LiveBook.Runtime.StandaloneInit

  @type t :: %__MODULE__{
          node: node(),
          primary_pid: pid(),
          init_ref: reference(),
          project_path: String.t()
        }

  @doc """
  Starts a new Elixir node (i.e. a system process) and initializes
  it with LiveBook-specific modules and processes.

  The node is started together with a Mix environment appropriate
  for the given `project_path`. The setup may involve
  long-running steps (like fetching dependencies, compiling the project),
  so the initialization is asynchronous. This function spawns and links
  a process responsible for initialization, which then sends
  notifications to the caller:

  * `{:runtime_init, {:output, string}}` - arbitrary output/info sent as the initialization proceeds
  * `{:runtime_init, {:ok, runtime}}` - a final message indicating successful initialization
  * `{:runtime_init, {:error, message}}` - a final message indicating failure

  The new node monitors the given owner process and terminates
  as soon as it terminates. It may also be terminated manually
  by using `Runtime.disconnect/1`.

  Note: to start the node it is required that `elixir` is a recognised
  executable within the system.
  """
  @spec init_async(pid(), String.t()) :: :ok
  def init_async(owner_pid, project_path) do
    stream_to = self()
    handle_output = fn output -> stream_info(stream_to, {:output, output}) end

    spawn_link(fn ->
      parent_node = node()
      {child_node, waiter} = init_parameteres()

      Utils.registered_as self(), waiter do
        with {:ok, elixir_path} <- find_elixir_executable(),
             :ok <- run_mix_task("deps.get", project_path, handle_output),
             :ok <- run_mix_task("compile", project_path, handle_output),
             eval <- child_node_ast(parent_node, waiter) |> Macro.to_string(),
             port <- start_elixir_node(elixir_path, child_node, eval, project_path),
             {:ok, primary_pid, init_ref} <-
               parent_init_sequence(child_node, port, owner_pid, handle_output) do
          runtime = %__MODULE__{
            node: child_node,
            primary_pid: primary_pid,
            init_ref: init_ref,
            project_path: project_path
          }

          stream_info(stream_to, {:ok, runtime})
        else
          {:error, error} ->
            stream_info(stream_to, {:error, error})
        end
      end
    end)

    :ok
  end

  defp run_mix_task(task, project_path, handle_output) do
    handle_output.("Running mix #{task}...\n")

    case System.cmd("mix", [task],
           cd: project_path,
           stderr_to_stdout: true,
           into: Utils.Callback.new(handle_output)
         ) do
      {_emitter, 0} -> :ok
      {_emitter, _status} -> {:error, "running mix #{task} failed, see output for more details"}
    end
  end

  defp start_elixir_node(elixir_path, node_name, eval, project_path) do
    # Here we create a port to start the system process in a non-blocking way.
    Port.open({:spawn_executable, elixir_path}, [
      :binary,
      :stderr_to_stdout,
      args: elixir_flags(node_name) ++ ["-S", "mix", "run", "--eval", eval],
      cd: project_path
    ])
  end

  defp stream_info(stream_to, message) do
    send(stream_to, {:runtime_init, message})
  end
end

defimpl LiveBook.Runtime, for: LiveBook.Runtime.MixStandalone do
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

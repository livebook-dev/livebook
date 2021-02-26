defmodule LiveBook.Runtime.MixStandalone do
  defstruct [:node, :primary_pid, :project_path]

  # A runtime backed by a standalone Elixir node managed by LiveBook.
  #
  # This runtime is similar to `LiveBook.Runtime.ElixirStandalone`,
  # but the node is started in the context of a Mix project.

  import LiveBook.Runtime.StandaloneInit

  alias LiveBook.Utils
  alias LiveBook.Utils.Emitter

  @type t :: %__MODULE__{
          node: node(),
          primary_pid: pid(),
          project_path: String.t()
        }

  @doc """
  Starts a new Elixir node (i.e. a system process) and initializes
  it with LiveBook-specific modules and processes.

  The node is started together with a Mix environment appropriate
  for the given `project_path`. The setup may involve
  long-running steps (like fetching dependencies, compiling the project),
  so the initialization is asynchronous. This function spawns and links
  a process responsible for initialization, which then uses `emitter`
  to emit the following notifications:

  * `{:output, string}` - arbitrary output/info sent as the initialization proceeds
  * `{:ok, runtime}` - a final message indicating successful initialization
  * `{:error, message}` - a final message indicating failure

  If no process calls `Runtime.connect/1` for a period of time,
  the node automatically terminates. Whoever connects, becomes the owner
  and as soon as it terminates, the node terminates as well.
  The node may also be terminated manually by using `Runtime.disconnect/1`.

  Note: to start the node it is required that both `elixir` and `mix` are
  recognised executables within the system.
  """
  @spec init_async(String.t(), Emitter.t()) :: :ok
  def init_async(project_path, emitter) do
    output_emitter = Emitter.mapper(emitter, fn output -> {:output, output} end)

    spawn_link(fn ->
      parent_node = node()
      child_node = random_node_name()
      parent_process_name = random_process_name()

      Utils.temporarily_register(self(), parent_process_name, fn ->
        with {:ok, elixir_path} <- find_elixir_executable(),
             :ok <- run_mix_task("deps.get", project_path, output_emitter),
             :ok <- run_mix_task("compile", project_path, output_emitter),
             eval <- child_node_ast(parent_node, parent_process_name) |> Macro.to_string(),
             port <- start_elixir_mix_node(elixir_path, child_node, eval, project_path),
             {:ok, primary_pid} <- parent_init_sequence(child_node, port, output_emitter) do
          runtime = %__MODULE__{
            node: child_node,
            primary_pid: primary_pid,
            project_path: project_path
          }

          Emitter.emit(emitter, {:ok, runtime})
        else
          {:error, error} ->
            Emitter.emit(emitter, {:error, error})
        end
      end)
    end)

    :ok
  end

  defp run_mix_task(task, project_path, output_emitter) do
    Emitter.emit(output_emitter, "Running mix #{task}...\n")

    case System.cmd("mix", [task],
           cd: project_path,
           stderr_to_stdout: true,
           into: output_emitter
         ) do
      {_callback, 0} -> :ok
      {_callback, _status} -> {:error, "running mix #{task} failed, see output for more details"}
    end
  end

  defp start_elixir_mix_node(elixir_path, node_name, eval, project_path) do
    # Here we create a port to start the system process in a non-blocking way.
    Port.open({:spawn_executable, elixir_path}, [
      :binary,
      :stderr_to_stdout,
      args: elixir_flags(node_name) ++ ["-S", "mix", "run", "--eval", eval],
      cd: project_path
    ])
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

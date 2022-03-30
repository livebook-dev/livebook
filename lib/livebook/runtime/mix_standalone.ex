defmodule Livebook.Runtime.MixStandalone do
  defstruct [:node, :server_pid, :project_path]

  # A runtime backed by a standalone Elixir node managed by Livebook.
  #
  # This runtime is similar to `Livebook.Runtime.ElixirStandalone`,
  # but the node is started in the context of a Mix project.

  import Livebook.Runtime.StandaloneInit

  alias Livebook.Utils
  alias Livebook.Utils.Emitter

  @type t :: %__MODULE__{
          node: node(),
          server_pid: pid(),
          project_path: String.t()
        }

  @doc """
  Starts a new Elixir node (i.e. a system process) and initializes
  it with Livebook-specific modules and processes.

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
      child_node = child_node_name(parent_node)

      Utils.temporarily_register(self(), child_node, fn ->
        argv = [parent_node]

        with {:ok, elixir_path} <- find_elixir_executable(),
             :ok <- run_mix_task("deps.get", project_path, output_emitter),
             :ok <- run_mix_task("compile", project_path, output_emitter),
             eval = child_node_eval_string(),
             port = start_elixir_mix_node(elixir_path, child_node, eval, argv, project_path),
             {:ok, server_pid} <- parent_init_sequence(child_node, port, emitter: output_emitter) do
          runtime = %__MODULE__{
            node: child_node,
            server_pid: server_pid,
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

  @doc """
  A synchronous version of of `init_async/2`.
  """
  @spec init(String.t()) :: {:ok, t()} | {:error, String.t()}
  def init(project_path) do
    %{ref: ref} = emitter = Livebook.Utils.Emitter.new(self())

    init_async(project_path, emitter)

    await_init(ref, [])
  end

  defp await_init(ref, outputs) do
    receive do
      {:emitter, ^ref, message} -> message
    end
    |> case do
      {:ok, runtime} ->
        {:ok, runtime}

      {:error, error} ->
        message = IO.iodata_to_binary([error, ". Output:\n\n", Enum.reverse(outputs)])
        {:error, message}

      {:output, output} ->
        await_init(ref, [output | outputs])
    end
  end

  defp run_mix_task(task, project_path, output_emitter) do
    Emitter.emit(output_emitter, "Running mix #{task}...\n")

    case System.cmd("mix", [task],
           cd: project_path,
           stderr_to_stdout: true,
           into: output_emitter
         ) do
      {_callback, 0} -> :ok
      {_callback, _status} -> {:error, "running mix #{task} failed"}
    end
  end

  defp start_elixir_mix_node(elixir_path, node_name, eval, argv, project_path) do
    # Here we create a port to start the system process in a non-blocking way.
    Port.open({:spawn_executable, elixir_path}, [
      :binary,
      # We don't communicate with the system process via stdio,
      # contrarily, we want any non-captured output to go directly
      # to the terminal
      :nouse_stdio,
      :hide,
      cd: project_path,
      args:
        elixir_flags(node_name) ++
          ["-S", "mix", "run", "--eval", eval, "--" | Enum.map(argv, &to_string/1)]
    ])
  end
end

defimpl Livebook.Runtime, for: Livebook.Runtime.MixStandalone do
  alias Livebook.Runtime.ErlDist

  def connect(runtime, opts \\ []) do
    ErlDist.RuntimeServer.attach(runtime.server_pid, self(), opts)
    Process.monitor(runtime.server_pid)
  end

  def disconnect(runtime) do
    ErlDist.RuntimeServer.stop(runtime.server_pid)
  end

  def evaluate_code(runtime, code, locator, base_locator, opts \\ []) do
    ErlDist.RuntimeServer.evaluate_code(runtime.server_pid, code, locator, base_locator, opts)
  end

  def forget_evaluation(runtime, locator) do
    ErlDist.RuntimeServer.forget_evaluation(runtime.server_pid, locator)
  end

  def drop_container(runtime, container_ref) do
    ErlDist.RuntimeServer.drop_container(runtime.server_pid, container_ref)
  end

  def handle_intellisense(runtime, send_to, ref, request, base_locator) do
    ErlDist.RuntimeServer.handle_intellisense(
      runtime.server_pid,
      send_to,
      ref,
      request,
      base_locator
    )
  end

  def duplicate(runtime) do
    Livebook.Runtime.MixStandalone.init(runtime.project_path)
  end

  def standalone?(_runtime), do: true

  def read_file(runtime, path) do
    ErlDist.RuntimeServer.read_file(runtime.server_pid, path)
  end

  def start_smart_cell(runtime, kind, ref, attrs, base_locator) do
    ErlDist.RuntimeServer.start_smart_cell(runtime.server_pid, kind, ref, attrs, base_locator)
  end

  def set_smart_cell_base_locator(runtime, ref, base_locator) do
    ErlDist.RuntimeServer.set_smart_cell_base_locator(runtime.server_pid, ref, base_locator)
  end

  def stop_smart_cell(runtime, ref) do
    ErlDist.RuntimeServer.stop_smart_cell(runtime.server_pid, ref)
  end

  def add_dependencies(_runtime, code, dependencies) do
    Livebook.Runtime.Code.add_mix_deps(code, dependencies)
  end
end

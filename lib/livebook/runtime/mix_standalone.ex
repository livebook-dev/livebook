defmodule Livebook.Runtime.MixStandalone do
  defstruct [:node, :server_pid, :project_path, :flags]

  # A runtime backed by a standalone Elixir node managed by Livebook.
  #
  # This runtime is similar to `Livebook.Runtime.ElixirStandalone`,
  # but the node is started in the context of a Mix project.

  import Livebook.Runtime.StandaloneInit

  alias Livebook.Utils
  alias Livebook.Utils.Emitter

  @type t :: %__MODULE__{
          project_path: String.t(),
          flags: String.t(),
          node: node() | nil,
          server_pid: pid() | nil
        }

  @doc """
  Returns a new runtime instance.
  """
  @spec new(String.t(), String.t()) :: t()
  def new(project_path, flags \\ "") do
    %__MODULE__{project_path: project_path, flags: flags}
  end

  @doc """
  Starts a new Elixir node (a system process) and initializes it with
  Livebook-specific modules and processes.

  The node is started together with a Mix environment at the given
  `project_path`. The setup may involve long-running steps (like
  fetching dependencies, compiling the project), so the initialization
  is asynchronous. This function spawns and links a process responsible
  for initialization, which then uses `emitter` to emit the following
  notifications:

    * `{:output, string}` - arbitrary output/info sent as the initialization proceeds
    * `{:ok, runtime}` - a final message indicating successful initialization
    * `{:error, message}` - a final message indicating failure

  Note: to start the node it is required that both `elixir` and `mix`
  are recognised executables within the system.
  """
  @spec connect_async(t(), Emitter.t()) :: :ok
  def connect_async(runtime, emitter) do
    %{project_path: project_path} = runtime
    flags = OptionParser.split(runtime.flags)
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
             port =
               start_elixir_mix_node(elixir_path, child_node, flags, eval, argv, project_path),
             {:ok, server_pid} <- parent_init_sequence(child_node, port, emitter: output_emitter) do
          runtime = %{runtime | node: child_node, server_pid: server_pid}
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
  A synchronous version of of `connect_async/2`.
  """
  @spec connect(t()) :: {:ok, t()} | {:error, String.t()}
  def connect(runtime) do
    %{ref: ref} = emitter = Livebook.Utils.Emitter.new(self())

    connect_async(runtime, emitter)

    await_connect(ref, [])
  end

  defp await_connect(ref, outputs) do
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
        await_connect(ref, [output | outputs])
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

  defp start_elixir_mix_node(elixir_path, node_name, flags, eval, argv, project_path) do
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
          ["-S", "mix", "run", "--eval", eval | flags] ++
          ["--" | Enum.map(argv, &to_string/1)]
    ])
  end
end

defimpl Livebook.Runtime, for: Livebook.Runtime.MixStandalone do
  alias Livebook.Runtime.ErlDist.RuntimeServer

  def describe(runtime) do
    [
      {"Type", "Mix standalone"},
      {"Project", runtime.project_path},
      runtime.flags != "" && {"Flags", runtime.flags},
      connected?(runtime) && {"Node name", Atom.to_string(runtime.node)}
    ]
    |> Enum.filter(&is_tuple/1)
  end

  def connect(runtime) do
    Livebook.Runtime.MixStandalone.connect(runtime)
  end

  def connected?(runtime) do
    runtime.server_pid != nil
  end

  def take_ownership(runtime, opts \\ []) do
    RuntimeServer.attach(runtime.server_pid, self(), opts)
    Process.monitor(runtime.server_pid)
  end

  def disconnect(runtime) do
    :ok = RuntimeServer.stop(runtime.server_pid)
    {:ok, %{runtime | node: nil, server_pid: nil}}
  end

  def duplicate(runtime) do
    Livebook.Runtime.MixStandalone.new(runtime.project_path, runtime.flags)
  end

  def evaluate_code(runtime, code, locator, base_locator, opts \\ []) do
    RuntimeServer.evaluate_code(runtime.server_pid, code, locator, base_locator, opts)
  end

  def forget_evaluation(runtime, locator) do
    RuntimeServer.forget_evaluation(runtime.server_pid, locator)
  end

  def drop_container(runtime, container_ref) do
    RuntimeServer.drop_container(runtime.server_pid, container_ref)
  end

  def handle_intellisense(runtime, send_to, request, base_locator) do
    RuntimeServer.handle_intellisense(runtime.server_pid, send_to, request, base_locator)
  end

  def read_file(runtime, path) do
    RuntimeServer.read_file(runtime.server_pid, path)
  end

  def start_smart_cell(runtime, kind, ref, attrs, base_locator) do
    RuntimeServer.start_smart_cell(runtime.server_pid, kind, ref, attrs, base_locator)
  end

  def set_smart_cell_base_locator(runtime, ref, base_locator) do
    RuntimeServer.set_smart_cell_base_locator(runtime.server_pid, ref, base_locator)
  end

  def stop_smart_cell(runtime, ref) do
    RuntimeServer.stop_smart_cell(runtime.server_pid, ref)
  end

  def fixed_dependencies?(_runtime), do: true

  def add_dependencies(_runtime, _code, _dependencies) do
    raise "not supported"
  end

  def search_packages(_runtime, _send_to, _search) do
    raise "not supported"
  end
end

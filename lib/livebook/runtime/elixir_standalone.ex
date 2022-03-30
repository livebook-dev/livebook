defmodule Livebook.Runtime.ElixirStandalone do
  defstruct [:node, :server_pid]

  # A runtime backed by a standalone Elixir node managed by Livebook.
  #
  # Livebook is responsible for starting and terminating the node.
  # Most importantly we have to make sure the started node doesn't
  # stay in the system when the session or the entire Livebook terminates.

  import Livebook.Runtime.StandaloneInit

  alias Livebook.Utils

  @type t :: %__MODULE__{
          node: node(),
          server_pid: pid()
        }

  kino_dep = {:kino, github: "livebook-dev/kino"}
  vega_lite_dep = {:vega_lite, "~> 0.1.3"}

  @extra_smart_cell_definitions [
    %{
      kind: "Elixir.Kino.SmartCell.DBConnection",
      name: "Database connection",
      requirement: %{name: "Kino", dependencies: [kino_dep]}
    },
    %{
      kind: "Elixir.Kino.SmartCell.SQL",
      name: "SQL query",
      requirement: %{name: "Kino", dependencies: [kino_dep]}
    },
    %{
      kind: "Elixir.Kino.SmartCell.ChartBuilder",
      name: "Chart builder",
      requirement: %{name: "Kino", dependencies: [kino_dep, vega_lite_dep]}
    }
  ]

  @doc """
  Starts a new Elixir node (i.e. a system process) and initializes
  it with Livebook-specific modules and processes.

  If no process calls `Runtime.connect/1` for a period of time,
  the node automatically terminates. Whoever connects, becomes the owner
  and as soon as it terminates, the node terminates as well.
  The node may also be terminated manually by using `Runtime.disconnect/1`.

  Note: to start the node it is required that `elixir` is a recognised
  executable within the system.
  """
  @spec init() :: {:ok, t()} | {:error, String.t()}
  def init() do
    parent_node = node()
    child_node = child_node_name(parent_node)

    Utils.temporarily_register(self(), child_node, fn ->
      argv = [parent_node]

      init_opts = [
        runtime_server_opts: [extra_smart_cell_definitions: @extra_smart_cell_definitions]
      ]

      with {:ok, elixir_path} <- find_elixir_executable(),
           port = start_elixir_node(elixir_path, child_node, child_node_eval_string(), argv),
           {:ok, server_pid} <- parent_init_sequence(child_node, port, init_opts: init_opts) do
        runtime = %__MODULE__{
          node: child_node,
          server_pid: server_pid
        }

        {:ok, runtime}
      else
        {:error, error} ->
          {:error, error}
      end
    end)
  end

  defp start_elixir_node(elixir_path, node_name, eval, argv) do
    # Here we create a port to start the system process in a non-blocking way.
    Port.open({:spawn_executable, elixir_path}, [
      :binary,
      # We don't communicate with the system process via stdio,
      # contrarily, we want any non-captured output to go directly
      # to the terminal
      :nouse_stdio,
      :hide,
      args: elixir_flags(node_name) ++ ["--eval", eval, "--" | Enum.map(argv, &to_string/1)]
    ])
  end
end

defimpl Livebook.Runtime, for: Livebook.Runtime.ElixirStandalone do
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

  def duplicate(_runtime) do
    Livebook.Runtime.ElixirStandalone.init()
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

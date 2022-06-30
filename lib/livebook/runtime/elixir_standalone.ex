defmodule Livebook.Runtime.ElixirStandalone do
  defstruct [:node, :server_pid]

  # A runtime backed by a standalone Elixir node managed by Livebook.
  #
  # Livebook is responsible for starting and terminating the node.
  # Most importantly we have to make sure the started node doesn't
  # stay in the system when the session or the entire Livebook
  # terminates.

  import Livebook.Runtime.StandaloneInit

  alias Livebook.Utils

  @type t :: %__MODULE__{
          node: node() | nil,
          server_pid: pid() | nil
        }

  kino_vega_lite = %{name: "kino_vega_lite", dependency: {:kino_vega_lite, "~> 0.1.1"}}
  kino_db = %{name: "kino_db", dependency: {:kino_db, "~> 0.1.2"}}
  kino_maplibre = %{name: "kino_maplibre", dependency: {:kino_maplibre, "~> 0.1.0"}}

  @extra_smart_cell_definitions [
    %{
      kind: "Elixir.KinoDB.ConnectionCell",
      name: "Database connection",
      requirement: %{
        variants: [
          %{
            name: "Amazon Athena",
            packages: [kino_db, %{name: "req_athena", dependency: {:req_athena, "~> 0.1.0"}}]
          },
          %{
            name: "Google BigQuery",
            packages: [kino_db, %{name: "req_bigquery", dependency: {:req_bigquery, "~> 0.1.0"}}]
          },
          %{
            name: "MySQL",
            packages: [kino_db, %{name: "myxql", dependency: {:myxql, "~> 0.6.2"}}]
          },
          %{
            name: "PostgreSQL",
            packages: [kino_db, %{name: "postgrex", dependency: {:postgrex, "~> 0.16.3"}}]
          },
          %{
            name: "SQLite",
            packages: [kino_db, %{name: "exqlite", dependency: {:exqlite, "~> 0.11.0"}}]
          }
        ]
      }
    },
    %{
      kind: "Elixir.KinoDB.SQLCell",
      name: "SQL query",
      requirement: %{
        variants: [
          %{
            name: "Default",
            packages: [kino_db]
          }
        ]
      }
    },
    %{
      kind: "Elixir.KinoVegaLite.ChartCell",
      name: "Chart",
      requirement: %{
        variants: [
          %{
            name: "Default",
            packages: [kino_vega_lite]
          }
        ]
      }
    },
    %{
      kind: "Elixir.KinoMapLibre.MapCell",
      name: "Map",
      requirement: %{
        variants: [
          %{
            name: "Default",
            packages: [kino_maplibre]
          }
        ]
      }
    }
  ]

  @doc """
  Returns a new runtime instance.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{}
  end

  @doc """
  Starts a new Elixir node (a system process) and initializes it with
  Livebook-specific modules and processes.

  If no process calls `Runtime.take_ownership/1` for a period of time,
  the node automatically terminates. Whoever takes the ownersihp,
  becomes the owner and as soon as it terminates, the node terminates
  as well. The node may also be terminated by calling `Runtime.disconnect/1`.

  Note: to start the node it is required that `elixir` is a recognised
  executable within the system.
  """
  @spec connect(t()) :: {:ok, t()} | {:error, String.t()}
  def connect(runtime) do
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
        runtime = %{runtime | node: child_node, server_pid: server_pid}
        {:ok, runtime}
      else
        {:error, error} -> {:error, error}
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
  alias Livebook.Runtime.ErlDist.RuntimeServer

  def describe(runtime) do
    [{"Type", "Elixir standalone"}] ++
      if connected?(runtime) do
        [{"Node name", Atom.to_string(runtime.node)}]
      else
        []
      end
  end

  def connect(runtime) do
    Livebook.Runtime.ElixirStandalone.connect(runtime)
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

  def duplicate(_runtime) do
    Livebook.Runtime.ElixirStandalone.new()
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

  def fixed_dependencies?(_runtime), do: false

  def add_dependencies(_runtime, code, dependencies) do
    Livebook.Runtime.Dependencies.add_mix_deps(code, dependencies)
  end

  def search_packages(_runtime, send_to, search) do
    Livebook.Runtime.Dependencies.search_packages_on_hex(send_to, search)
  end
end

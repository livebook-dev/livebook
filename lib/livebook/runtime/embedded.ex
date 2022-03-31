defmodule Livebook.Runtime.Embedded do
  @moduledoc false

  # A runtime backed by the same node Livebook is running in.
  #
  # This runtime is reserved for specific use cases,
  # where there is no option of starting a separate
  # Elixir runtime.

  defstruct [:server_pid]

  @type t :: %__MODULE__{server_pid: pid()}

  alias Livebook.Runtime.ErlDist

  @doc """
  Initializes new runtime by starting the necessary
  processes within the current node.
  """
  @spec init() :: {:ok, t()}
  def init() do
    # As we run in the Livebook node, all the necessary modules
    # are in place, so we just start the manager process.
    # We make it anonymous, so that multiple embedded runtimes
    # can be started (for different notebooks).
    # We also disable cleanup, as we don't want to unload any
    # modules or revert the configuration (because other runtimes
    # may rely on it). If someone uses embedded runtimes,
    # this cleanup is not particularly important anyway.
    # We tell manager to not override :standard_error,
    # as we already do it for the Livebook application globally
    # (see Livebook.Application.start/2).

    server_pid =
      ErlDist.initialize(node(),
        node_manager_opts: [unload_modules_on_termination: false]
      )

    {:ok, %__MODULE__{server_pid: server_pid}}
  end
end

defimpl Livebook.Runtime, for: Livebook.Runtime.Embedded do
  alias Livebook.Runtime.ErlDist

  def describe(_runtime) do
    [{"Type", "Embedded"}]
  end

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
    Livebook.Runtime.Embedded.init()
  end

  def standalone?(_runtime), do: false

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

  def fixed_dependencies?(_runtime), do: true

  def add_dependencies(_runtime, _code, _dependencies) do
    raise "not supported"
  end

  def search_dependencies(_runtime, _send_to, _search) do
    raise "not supported"
  end
end

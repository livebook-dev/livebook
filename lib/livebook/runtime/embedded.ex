defmodule Livebook.Runtime.Embedded do
  @moduledoc false

  # A runtime backed by the same node Livebook is running in.
  #
  # This runtime is reserved for specific use cases, where there is
  # no option of starting a separate Elixir runtime.

  defstruct [:server_pid]

  @type t :: %__MODULE__{server_pid: pid() | nil}

  alias Livebook.Runtime.ErlDist

  @doc """
  Returns a new runtime instance.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{}
  end

  @doc """
  Initializes new runtime by starting the necessary processes within
  the current node.
  """
  @spec connect(t()) :: {:ok, t()}
  def connect(runtime) do
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

    {:ok, %{runtime | server_pid: server_pid}}
  end
end

defimpl Livebook.Runtime, for: Livebook.Runtime.Embedded do
  alias Livebook.Runtime.ErlDist.RuntimeServer

  def describe(_runtime) do
    [{"Type", "Embedded"}]
  end

  def connect(runtime) do
    Livebook.Runtime.Embedded.connect(runtime)
  end

  def connected?(runtime) do
    runtime.server_pid != nil
  end

  def take_ownership(runtime, opts \\ []) do
    RuntimeServer.attach(runtime.server_pid, self(), opts)
    Process.monitor(runtime.server_pid)
  end

  def disconnect(runtime) do
    RuntimeServer.stop(runtime.server_pid)
    {:ok, %{runtime | server_pid: nil}}
  end

  def duplicate(_runtime) do
    Livebook.Runtime.Embedded.new()
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

  def fixed_dependencies?(_runtime) do
    not Keyword.has_key?(config(), :load_packages)
  end

  def add_dependencies(_runtime, code, dependencies) do
    Livebook.Runtime.Dependencies.add_mix_deps(code, dependencies)
  end

  def search_packages(_runtime, send_to, search) do
    {mod, fun, args} = config()[:load_packages]
    packages = apply(mod, fun, args)
    Livebook.Runtime.Dependencies.search_packages_in_list(packages, send_to, search)
  end

  defp config() do
    Application.get_env(:livebook, Livebook.Runtime.Embedded, [])
  end
end

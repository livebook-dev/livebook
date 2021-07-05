defmodule Livebook.Runtime.Embedded do
  @moduledoc false

  # A runtime backed by the same node Livebook is running in.
  #
  # This runtime is reserved for specific use cases,
  # where there is no option of starting a separate
  # Elixir runtime.

  defstruct [:node, :server_pid]

  @type t :: %__MODULE__{
          node: node(),
          server_pid: pid()
        }

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

    server_pid = ErlDist.initialize(node(), unload_modules_on_termination: false)
    {:ok, %__MODULE__{node: node(), server_pid: server_pid}}
  end
end

defimpl Livebook.Runtime, for: Livebook.Runtime.Embedded do
  alias Livebook.Runtime.ErlDist

  def connect(runtime) do
    ErlDist.RuntimeServer.set_owner(runtime.server_pid, self())
    Process.monitor(runtime.server_pid)
  end

  def disconnect(runtime) do
    ErlDist.RuntimeServer.stop(runtime.server_pid)
  end

  def evaluate_code(
        runtime,
        code,
        container_ref,
        evaluation_ref,
        prev_evaluation_ref,
        opts \\ []
      ) do
    ErlDist.RuntimeServer.evaluate_code(
      runtime.server_pid,
      code,
      container_ref,
      evaluation_ref,
      prev_evaluation_ref,
      opts
    )
  end

  def forget_evaluation(runtime, container_ref, evaluation_ref) do
    ErlDist.RuntimeServer.forget_evaluation(runtime.server_pid, container_ref, evaluation_ref)
  end

  def drop_container(runtime, container_ref) do
    ErlDist.RuntimeServer.drop_container(runtime.server_pid, container_ref)
  end

  def request_completion_items(runtime, send_to, ref, hint, container_ref, evaluation_ref) do
    ErlDist.RuntimeServer.request_completion_items(
      runtime.server_pid,
      send_to,
      ref,
      hint,
      container_ref,
      evaluation_ref
    )
  end

  def duplicate(_runtime) do
    Livebook.Runtime.Embedded.init()
  end
end

defmodule LiveBook.Remote do
  @moduledoc false

  # This module allows for initializing connected nodes
  # with modules and processes necessary for evaluation.
  #
  # To ensure proper isolation between sessions,
  # code evaluation takes place in a separate Elixir runtime.
  # This also makes it easy to terminate the whole
  # evaluation environment without stopping LiveBook.
  #
  # As each session has a corresponding node for evaluation
  # we have a distributed system, so we can leverage
  # Erlang capabilities for their communication.
  #
  # By abstracting evaluation away to an arbitrary node we naturally get
  # support for evaluation in the context of existing nodes,
  # like those started in a mix environment.
  #
  # To work with a separate node, we have to inject the necessary
  # LiveBook modules there and also start the relevant processes
  # related to evaluation. Fortunately Erlang allows us to send modules
  # binary representation to the other node and load them dynamically.

  alias LiveBook.Remote.{InitializationCounter, EvaluatorSupervisor}

  # Modules to load into the connected node.
  @required_modules [
    LiveBook.Evaluator,
    LiveBook.Evaluator.IOProxy,
    LiveBook.Remote.EvaluatorSupervisor,
    LiveBook.Remote.InitializationCounter
  ]

  @doc """
  Loads the necessary modules into the given node
  and starts a LiveBook-related supervision tree.

  The initialization may be invoked multiple times for the same node,
  in which case no double work is done, but the node keeps track
  of the number of initializations. Each call to this function
  should indicate a separate session willing to use the node.
  """
  @spec initialize(node()) :: :ok
  def initialize(node) do
    unless initialized?(node) do
      load_required_modules(node)
      start_supervisor(node)
    end

    InitializationCounter.increment(node)

    :ok
  end

  @doc """
  Unloads LiveBook-specific modules from the given node
  and tears down the supervision tree.

  If initialization has been invoked N times for the given node,
  only N-th call to this function will actually perform the cleanup.
  This way if multiple sessions are connected to the same node,
  the deinitialization happens once all of them are disconnected.
  """
  @spec deinitialize(node()) :: :ok
  def deinitialize(node) do
    InitializationCounter.decrement(node)

    if InitializationCounter.value(node) == 0 do
      unload_required_modules(node)
      stop_supervisor(node)
    end

    :ok
  end

  defp load_required_modules(node) do
    for module <- @required_modules do
      {_module, binary, filename} = :code.get_object_code(module)
      {:module, _} = :rpc.call(node, :code, :load_binary, [module, filename, binary])
    end
  end

  defp unload_required_modules(node) do
    for module <- @required_modules do
      :rpc.call(node, :code, :delete, [module])
      :rpc.call(node, :code, :purge, [module])
    end
  end

  defp start_supervisor(node) do
    children = [
      # Start the supervisor to dynamically manage evaluators
      EvaluatorSupervisor,
      # Start the process keeping track of the number of connected sessions
      InitializationCounter
    ]

    opts = [strategy: :one_for_one, name: LiveBook.Remote.Supervisor]

    {:ok, _pid} = :rpc.block_call(node, Supervisor, :start_link, [children, opts])
  end

  defp stop_supervisor(node) do
    :rpc.call(node, Supervisor, :stop, [LiveBook.Remote.Supervisor, :normal])
  end

  defp initialized?(node) do
    case :rpc.call(node, Process, :whereis, [LiveBook.Remote.Supervisor]) do
      nil -> false
      _pid -> true
    end
  end
end

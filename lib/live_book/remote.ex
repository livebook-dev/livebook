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

  # Modules to load into the connected node.
  @required_modules [
    LiveBook.Evaluator,
    LiveBook.Evaluator.IOProxy,
    LiveBook.Remote,
    LiveBook.Remote.Manager,
    LiveBook.Remote.EvaluatorSupervisor
  ]

  @doc """
  Loads the necessary modules into the given node
  and starts the primary LiveBook remote process.

  The initialization may be invoked only once on the given
  node until its disconnected.
  """
  @spec initialize(node()) :: :ok
  def initialize(node) do
    if initialized?(node) do
      # TODO: return eror tuple
      raise "already initialized"
    else
      load_required_modules(node)
      start_manager(node)
    end

    :ok
  end

  def load_required_modules(node) do
    for module <- @required_modules do
      {_module, binary, filename} = :code.get_object_code(module)
      {:module, _} = :rpc.call(node, :code, :load_binary, [module, filename, binary])
    end
  end

  def unload_required_modules() do
    for module <- @required_modules do
      # If we attached, detached and attached again, there may still
      # be deleted module code, so purge it first.
      :code.purge(module)
      :code.delete(module)
    end
  end

  defp start_manager(node) do
    # TODO: fix owner pid
    :rpc.call(node, LiveBook.Remote.Manager, :start, [[owner_pid: self()]])
  end

  defp initialized?(node) do
    case :rpc.call(node, Process, :whereis, [LiveBook.Remote.Manager]) do
      nil -> false
      _pid -> true
    end
  end
end

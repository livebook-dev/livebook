defmodule LiveBook.Runtime.StandaloneInit do
  @moduledoc false

  # Generic functionality related to starting and setting up
  # a new Elixir system process. It's used by both ElixirStandalone
  # and MixStandalone runtimes.

  alias LiveBook.Utils

  def init_parameteres() do
    id = Utils.random_short_id()
    child_node = Utils.node_from_name("live_book_runtime_#{id}")
    # We have to pass parent process pid to the new Elixir node.
    # The node receives code to evaluate as string, so we cannot
    # directly embed the pid there, but we can temporarily register
    # the process under a random name and pass this name to the child node.
    parent_process_name = :"live_book_parent_process_name_#{id}"

    {child_node, parent_process_name}
  end

  def find_elixir_executable() do
    case System.find_executable("elixir") do
      nil -> {:error, "no Elixir executable found in PATH"}
      path -> {:ok, path}
    end
  end

  def elixir_flags(node_name) do
    [
      if(LiveBook.Config.shortnames?(), do: "--sname", else: "--name"),
      to_string(node_name),
      "--erl",
      # Minimize shedulers busy wait threshold,
      # so that they go to sleep immediately after evaluation.
      # Enable ANSI escape codes as we handle them with HTML.
      "+sbwt none +sbwtdcpu none +sbwtdio none -elixir ansi_enabled true"
    ]
  end

  # The process proceedes as follows:
  #
  # 1. Waits for the child node to send an initial message.
  # 2. Initializes the remote node with necessary modules and processes.
  # 3. Responds to the child node indicating that initialization has finished (finishdes handshake).
  #
  # Handles timeouts and unexpected crashes.
  # Returns {:ok, primary_pid} or {:error, message}.
  def parent_init_sequence(child_node, port, handle_output \\ fn _ -> :ok end) do
    port_ref = Port.monitor(port)

    loop = fn loop ->
      receive do
        {:node_started, init_ref, ^child_node, primary_pid} ->
          Port.demonitor(port_ref)

          # We've just created the node, so it is surely not in use
          :ok = LiveBook.Runtime.ErlDist.initialize(child_node)

          send(primary_pid, {:node_initialized, init_ref})

          {:ok, primary_pid}

        {^port, {:data, output}} ->
          handle_output.(output)
          loop.(loop)

        {:DOWN, ^port_ref, :port, _object, _reason} ->
          {:error, "Elixir process terminated unexpectedly"}
      after
        10_000 ->
          {:error, "connection timed out"}
      end
    end

    loop.(loop)
  end

  # The process proceedes as follows:
  #
  # 1. Sends an initial message to the parent node to establish communication.
  # 2. Waits for the parent node to initialize this node and send a message back (handshake).
  # 3. Starts monitoring the specified the Manager process and freezes
  #    until it terminates or an explicit stop request is sent.
  #
  # Handles timeouts and unexpected crashes.
  def child_node_ast(parent_node, parent_process_name) do
    # This is the code that's gonna be evaluated in the newly
    # spawned Elixir runtime. This is the primary process
    # and as soon as it finishes, the runtime terminates.
    quote do
      # Initiate communication with the waiting process on the parent node.
      init_ref = make_ref()

      send(
        {unquote(parent_process_name), unquote(parent_node)},
        {:node_started, init_ref, node(), self()}
      )

      receive do
        {:node_initialized, ^init_ref} ->
          manager_ref = Process.monitor(LiveBook.Runtime.ErlDist.Manager)

          # Wait until the Manager process terminates.
          receive do
            {:DOWN, ^manager_ref, :process, _object, _reason} ->
              :ok
          end
      after
        10_000 -> :timeout
      end
    end
  end
end

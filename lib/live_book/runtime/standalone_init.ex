defmodule LiveBook.Runtime.StandaloneInit do
  @moduledoc false

  # Generic functionality related to starting and setting up
  # a new Elixir system process. It's used by both ElixirStandalone
  # and MixStandalone runtimes.

  alias LiveBook.Utils

  def init_parameteres() do
    id = Utils.random_short_id()
    node = Utils.node_from_name("live_book_runtime_#{id}")
    # The new Elixir node receives a code to evaluate
    # and we have to pass the current pid there, but since pid
    # is not a type we can include directly in the code,
    # we have to register the current process under a name.
    waiter = :"live_book_waiter_#{id}"

    {node, waiter}
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
  # 2. Responds with acknowledgement (handshake).
  # 3. Initializes the remote node with necessary modules and processes.
  #
  # Handles timeouts and unexpected crashes.
  # Returns {:ok, primary_pid, init_ref} or {:error, message}.
  def parent_init_sequence(child_node, port, owner_pid, handle_output \\ fn _ -> :ok end) do
    port_ref = Port.monitor(port)

    loop = fn loop ->
      receive do
        {:node_started, init_ref, ^child_node, primary_pid} ->
          Port.demonitor(port_ref)

          # Having the other process pid we can send the owner pid as a message.
          send(primary_pid, {:node_acknowledged, init_ref, owner_pid})

          # There should be no problem initializing the new node
          :ok = LiveBook.Runtime.ErlDist.initialize(child_node)

          {:ok, primary_pid, init_ref}

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
  # 2. Waits for the parent node to send acknowledgement (handshake).
  # 3. Starts monitoring the specified owner process and freezes
  #    until it terminates or an explicit stop request is sent.
  #
  # Handles timeouts and unexpected crashes.
  def child_node_ast(parent_node, waiter) do
    # This is the code that's gonna be evaluated in the newly
    # spawned Elixir runtime. This is the primary process
    # and as soon as it finishes, the runtime terminates.
    quote do
      # Initiate communication with the waiting process on the parent node.
      init_ref = make_ref()
      send({unquote(waiter), unquote(parent_node)}, {:node_started, init_ref, node(), self()})

      receive do
        {:node_acknowledged, ^init_ref, owner_pid} ->
          owner_ref = Process.monitor(owner_pid)

          # Wait until either the owner process terminates
          # or we receives an explicit stop request.
          receive do
            {:DOWN, ^owner_ref, :process, _object, _reason} ->
              :ok

            {:stop, ^init_ref} ->
              :ok
          end
      after
        10_000 -> :timeout
      end
    end
  end
end

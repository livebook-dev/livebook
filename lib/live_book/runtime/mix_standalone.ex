defmodule LiveBook.Runtime.MixStandalone do
  defstruct [:node, :primary_pid, :init_ref, :project_path]

  # A runtime backed by a standalone Elixir node managed by LiveBook.
  #
  # LiveBook is responsible for starting and terminating the node.
  # Most importantly we have to make sure the started node doesn't
  # stay in the system when the session or the entire LiveBook terminates.

  alias LiveBook.Utils
  require LiveBook.Utils

  @type t :: %__MODULE__{
          node: node(),
          primary_pid: pid(),
          init_ref: reference(),
          project_path: String.t()
        }

  @doc """
  Starts a new Elixir node (i.e. a system process) and initializes
  it with LiveBook-specific modules and processes.

  The new node monitors the given owner process and terminates
  as soon as it terminates. It may also be terminated manually
  by using `Runtime.disconnect/1`.

  Note: to start the node it is required that `elixir` is a recognised
  executable within the system.
  """
  @spec init_async(pid(), String.t()) :: :ok
  def init_async(owner_pid, project_path) do
    stream_to = self()

    spawn_link(fn ->
      parent_node = node()
      {child_node, waiter} = init_parameteres()

      Utils.registered_as self(), waiter do
        with {:ok, elixir_path} <- find_elixir_executable(),
             :ok <- run_mix_task("deps.get", project_path, stream_to),
             :ok <- run_mix_task("compile", project_path, stream_to),
             eval <- child_node_eval_ast(parent_node, waiter) |> Macro.to_string(),
             port <- start_elixir_node(elixir_path, child_node, eval, project_path),
             {:ok, primary_pid, init_ref} <- parent_init_sequence(child_node, port, owner_pid, stream_to) do
          runtime = %__MODULE__{
            node: child_node,
            primary_pid: primary_pid,
            init_ref: init_ref,
            project_path: project_path
          }

          stream_info(stream_to, {:ok, runtime})
        else
          {:error, error} ->
            stream_info(stream_to, {:error, error})
        end
      end
    end)

    :ok
  end

  defp init_parameteres() do
    id = Utils.random_short_id()
    node = Utils.node_from_name("live_book_runtime_#{id}")
    # The new Elixir node receives a code to evaluate
    # and we have to pass the current pid there, but since pid
    # is not a type we can include directly in the code,
    # we temporarily register the current process under a name.
    waiter = :"live_book_waiter_#{id}"

    {node, waiter}
  end

  defp find_elixir_executable() do
    case System.find_executable("elixir") do
      nil -> {:error, "no Elixir executable found in PATH"}
      path -> {:ok, path}
    end
  end

  defp run_mix_task(task, project_path, stream_to) do
    stream_info(stream_to, {:output, "Running mix #{task}...\n"})

    case System.cmd("mix", [task],
           cd: project_path,
           stderr_to_stdout: true,
           into: Utils.MessageEmitter.new(stream_to, fn output ->
             {:runtime_init, {:output, output}}
           end)
         ) do
      {_emitter, 0} -> :ok
      {_emitter, _status} -> {:error, "running mix #{task} failed, see output for more details"}
    end
  end

  defp start_elixir_node(elixir_path, node_name, eval, project_path) do
    # Here we create a port to start the system process in a non-blocking way.
    Port.open({:spawn_executable, elixir_path}, [
      :binary,
      :stderr_to_stdout,
      args: [
        if(LiveBook.Config.shortnames?(), do: "--sname", else: "--name"),
        to_string(node_name),
        # Minimize shedulers busy wait threshold,
        # so that they go to sleep immediately after evaluation.
        # Enable ANSI escape codes as we handle them with HTML.
        "--erl",
        "+sbwt none +sbwtdcpu none +sbwtdio none -elixir ansi_enabled true",
        "-S",
        "mix",
        "run",
        "--eval",
        eval
      ],
      cd: project_path
    ])
  end

  # The process proceedes as follows:
  #
  # 1. Waits for the child node to send an initial message.
  # 2. Responds with acknowledgement (handshake).
  # 3. Initializes the remote node with necessary modules and processes.
  #
  # Handles timeouts and unexpected crashes.
  # Returns {:ok, primary_pid, init_ref} or {:error, message}.
  defp parent_init_sequence(child_node, port, owner_pid, stream_to) do
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
          stream_info(stream_to, {:output, output})
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
  defp child_node_eval_ast(parent_node, waiter) do
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

  defp stream_info(stream_to, message) do
    send(stream_to, {:runtime_init, message})
  end
end

defimpl LiveBook.Runtime, for: LiveBook.Runtime.MixStandalone do
  alias LiveBook.Runtime.ErlDist

  def connect(runtime) do
    ErlDist.Manager.set_owner(runtime.node, self())
    Process.monitor({ErlDist.Manager, runtime.node})
  end

  def disconnect(runtime) do
    ErlDist.Manager.stop(runtime.node)
    # Instruct the other node to terminate
    send(runtime.primary_pid, {:stop, runtime.init_ref})
  end

  def evaluate_code(runtime, code, container_ref, evaluation_ref, prev_evaluation_ref \\ :initial) do
    ErlDist.Manager.evaluate_code(
      runtime.node,
      code,
      container_ref,
      evaluation_ref,
      prev_evaluation_ref
    )
  end

  def forget_evaluation(runtime, container_ref, evaluation_ref) do
    ErlDist.Manager.forget_evaluation(runtime.node, container_ref, evaluation_ref)
  end

  def drop_container(runtime, container_ref) do
    ErlDist.Manager.drop_container(runtime.node, container_ref)
  end
end

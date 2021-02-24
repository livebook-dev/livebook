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
  @spec init(pid(), String.t()) :: :ok
  def init(owner_pid, project_path) do
    stream_to = self()

    spawn_link(fn ->
      with {:ok, elixir_path} <- find_elixir_executable(stream_to),
           :ok <- run_mix_task(stream_to, "deps.get", project_path),
           :ok <- run_mix_task(stream_to, "compile", project_path) do
        id = Utils.random_short_id()
        node = Utils.node_from_name("live_book_runtime_#{id}")
        # The new Elixir node receives a code to evaluate
        # and we have to pass the current pid there, but since pid
        # is not a type we can include directly in the code,
        # we temporarily register the current process under a name.
        waiter = :"live_book_waiter_#{id}"

        Utils.registered_as self(), waiter do
          eval = child_node_eval(waiter, node()) |> Macro.to_string()
          port = start_elixir_node(elixir_path, node, eval, project_path)

          with {:ok, primary_pid, init_ref} <- parent_node_init(stream_to, node, port, owner_pid) do
            runtime = %__MODULE__{node: node, primary_pid: primary_pid, init_ref: init_ref, project_path: project_path}
            send(stream_to, {:runtime_init, {:ok, runtime}})
          end
        end
      end
    end)

    :ok
  end

  defp find_elixir_executable(stream_to) do
    case System.find_executable("elixir") do
      nil ->
        send(stream_to, {:runtime_init, {:error, :no_elixir_executable}})
        :error

      path ->
        {:ok, path}
    end
  end

  # TODO: add description
  defmodule Caller do
    defstruct [:pid]
  end

  defimpl Collectable, for: Caller do
    def into(original) do
      collector_fun = fn
        caller, {:cont, output} ->
          send(caller.pid, {:runtime_init, {:output, output}})
          caller
        caller, :done -> caller
        _caller, :halt -> :ok
      end

      {original, collector_fun}
    end
  end

  defp run_mix_task(stream_to, task, project_path) do
    send(stream_to, {:runtime_init, {:output, "Running mix #{task}...\n"}})

    case System.cmd("mix", [task], cd: project_path, stderr_to_stdout: true, into: %Caller{pid: stream_to}) do
      {_output, 0} ->
        :ok

      {_output, _status} ->
        send(stream_to, {:runtime_init, {:error, :failed}})
        :error
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

  defp parent_node_init(stream_to, node, port, owner_pid) do
    port_ref = Port.monitor(port)

    loop = fn loop ->
      receive do
        {:node_started, init_ref, ^node, primary_pid} ->
          Port.demonitor(port_ref)

          # Having the other process pid we can send the owner pid as a message.
          send(primary_pid, {:node_acknowledged, init_ref, owner_pid})

          # There should be no problem initializing the new node
          :ok = LiveBook.Runtime.ErlDist.initialize(node)

          {:ok, primary_pid, init_ref}

        {^port, {:data, output}} ->
          send(stream_to, {:runtime_init, {:output, output}})
          loop.(loop)

        {:DOWN, ^port_ref, :port, _object, _reason} ->
          {:error, :external}
      after
        10_000 ->
          {:error, :timeout}
      end
    end

    loop.(loop)
  end

  defp child_node_eval(waiter, parent_node) do
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

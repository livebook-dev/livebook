defmodule LiveBook.Remote do
  defstruct [:node, :primary_pid, :type]

  alias LiveBook.Utils

  @type t :: %__MODULE__{
          node: node(),
          primary_pid: pid() | nil,
          type: remote_type()
        }

  @type remote_type :: :internal | :external

  @node :"live_book@127.0.0.1"

  @required_modules [LiveBook.Evaluator, LiveBook.Evaluator.IOProxy, LiveBook.EvaluatorSupervisor]

  def ensure_distribution() do
    unless Node.alive?() do
      Node.start(@node, :longnames)
    end

    :ok
  end

  def start() do
    id = Utils.random_id()
    node = :"live_book_remote_#{id}@127.0.0.1"

    waiter = :"live_book_waiter_#{id}"
    Process.register(self(), waiter)

    eval =
      quote do
        parent_process = {unquote(waiter), unquote(@node)}
        ref = Process.monitor(parent_process)
        send(parent_process, {:node_started, node(), self()})

        receive do
          {:DOWN, ^ref, :process, _object, _reason} ->
            :ok

          :stop ->
            :ok
        end
      end
      |> Macro.to_string()

    elixir_path = System.find_executable("elixir")
    # Pass the nouse_stdio option, so that the caller does not
    # receive unexpected messages if the process produces some output.
    Port.open({:spawn_executable, elixir_path}, [:nouse_stdio, args: ["--name", to_string(node), "--eval", eval]])

    # TODO: timeout or something with error?
    primary_pid =
      receive do
        {:node_started, ^node, primary_pid} ->
          Process.unregister(waiter)
          primary_pid
      end

    %__MODULE__{node: node, primary_pid: primary_pid, type: :internal}
  end

  def initialize(remote) do
    load_required_modules(remote.node)
    start_supervisor(remote.node)
    :ok
  end

  def deinitialize(remote) do
    unload_required_modules(remote.node)
    stop_supervisor(remote.node)
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
    children = [LiveBook.EvaluatorSupervisor]
    opts = [strategy: :one_for_one, name: LiveBook.Remote.Supervisor]
    {:ok, _pid} = :rpc.block_call(node, Supervisor, :start_link, [children, opts])
  end

  defp stop_supervisor(node) do
    :rpc.call(node, Supervisor, :stop, [LiveBook.Remote.Supervisor, :normal])
  end

  def stop(%{type: :internal} = remote) do
    send(remote.primary_pid, :stop)
    :ok
  end

  def stop(%{type: :external} = _remote) do
    :ok
  end
end

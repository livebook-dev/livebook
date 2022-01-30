defmodule Livebook.UniqueTask.Task do
  @moduledoc false

  use GenServer, restart: :temporary

  @registry Livebook.UniqueTask.Registry

  def start_link({key, fun}) do
    GenServer.start_link(__MODULE__, fun, name: {:via, Registry, {@registry, key}})
  end

  @impl true
  def init(fun) do
    {:ok, nil, {:continue, fun}}
  end

  @impl true
  def handle_continue(fun, state) do
    fun.()
    {:stop, :shutdown, state}
  end
end

defmodule Livebook.UniqueTask do
  @moduledoc false

  use Supervisor

  @registry Livebook.UniqueTask.Registry
  @supervisor Livebook.UniqueTask.Supervisor
  @task Livebook.UniqueTask.Task

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, {}, name: __MODULE__)
  end

  @impl true
  def init({}) do
    children = [
      {Registry, name: @registry, keys: :unique},
      {DynamicSupervisor, name: @supervisor, strategy: :one_for_one}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

  @doc """
  Runs the given function in a separate process,
  unless the key is already taken.

  If another function is already running under the
  given key, this call only waits for it to finish
  and then returns the same status.

  Returns `:ok` if function finishes successfully and
  `:error` if it crashes.
  """
  @spec run(term(), function()) :: :ok | :error
  def run(key, fun) do
    pid =
      case Registry.lookup(@registry, key) do
        [{pid, _}] ->
          pid

        [] ->
          case DynamicSupervisor.start_child(@supervisor, {@task, {key, fun}}) do
            {:ok, pid} -> pid
            {:error, {:already_started, pid}} -> pid
          end
      end

    ref = Process.monitor(pid)

    receive do
      {:DOWN, ^ref, :process, ^pid, reason} ->
        case reason do
          :normal -> :ok
          :shutdown -> :ok
          {:shutdown, _} -> :ok
          :noproc -> :ok
          _ -> :error
        end
    end
  end
end

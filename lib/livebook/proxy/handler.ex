defmodule Livebook.Proxy.Handler do
  @moduledoc false

  # Handles request `conn` with the configured plug.
  #
  # The handler forwards all actual communication to the parent
  # `Livebook.Proxy.Server` via `Livebook.Proxy.Adapter`.
  #
  # A handler process is started on demand under a task supervisor.
  # To avoid bottlenecks, we use a partition supervisor, so that we
  # have a group of task supervisors ready.

  @name __MODULE__

  @doc """
  Returns a child spec to setup the handler supervision tree.

  Expects the `:listen` option to be provided, and be one of:

    * a function plug: `Plug.Conn.t() -> Plug.Conn.t()`

    * a module plug: `module` atom or `{module, options}` tuple.
  """
  @spec child_spec(keyword()) :: Supervisor.child_spec()
  def child_spec(opts) do
    listen = Keyword.fetch!(opts, :listen)
    :persistent_term.put({__MODULE__, :listen}, listen)
    PartitionSupervisor.child_spec(child_spec: Task.Supervisor, name: @name)
  end

  @doc """
  Handles request with the configured listener function.

  Restores `%Plug.Conn{}` from the given attributes and delegates
  all response handling back to the parent `Livebook.Proxy.Server`.
  """
  @spec serve(pid(), map()) :: Plug.Conn.t()
  def serve(parent_pid, %{} = conn_attrs) when is_pid(parent_pid) do
    Process.link(parent_pid)
    ref = Process.monitor(parent_pid)

    conn =
      struct!(Plug.Conn, %{conn_attrs | adapter: {Livebook.Proxy.Adapter, {parent_pid, ref}}})

    case :persistent_term.get({__MODULE__, :listen}) do
      fun when is_function(fun, 1) ->
        fun.(conn)

      mod when is_atom(mod) ->
        mod.call(conn, mod.init([]))

      {mod, opts} when is_atom(mod) ->
        mod.call(conn, mod.init(opts))

      other ->
        raise """
        expected plug to be one of:

          * fun(conn)
          * module
          * {module, options}

        got: #{inspect(other)}
        """
    end
  end

  @doc """
  Returns a pid of task supervisor to start the handler under.

  In case no supervisor is running, returns `nil`.
  """
  @spec get_supervisor_pid() :: pid() | nil
  def get_supervisor_pid() do
    if Process.whereis(@name) do
      key = :rand.uniform()
      GenServer.whereis({:via, PartitionSupervisor, {@name, key}})
    end
  end
end

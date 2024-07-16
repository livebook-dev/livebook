defmodule Livebook.Runtime.Fly do
  # A runtime backed by a Fly.io machine managed by Livebook.
  #
  # This runtime uses a Livebook-managed Elixir node, similarly to
  # the Standalone runtime, however it runs on a temporary Fly.io
  # machine. The machine is configured to automatically shutdown
  # as soon as the runtime is disconnected.
  #
  # Note: this runtime requires `flyctl` executable to be available
  # in the system.
  #
  # ## Communication
  #
  # The machine runs the Livebook Docker image and we configure it to
  # invoke the start_runtime.exs script, by setting LIVEBOOK_RUNTIME.
  # This environment variable also includes encoded information passed
  # from the parent. Once the Elixir node starts on the machine, it
  # waits for the parent to connect and finish the initialization.
  #
  # Now, we want to establish a distribution connection from the local
  # Livebook node to the node on the Fly.io machine. We could reach
  # the node directly, by requiring the user to set up WireGuard.
  # However, that would require the user to install WireGuard and go
  # through a few configuration steps. Instead, we use flyctl proxy
  # feature and only require flyctl to be installed.
  #
  # With flyctl proxy, we proxy a local port to the the distribution
  # port of the Fly.io node. Then, in our EPMD module (`Livebook.EPMD`)
  # we special case those nodes in two ways: (1) we infer the
  # distribution port from the node name; (2) we resolve the node
  # address to loopback, ignoring its hostname.
  #
  # ### Distribution protocol
  #
  # Usually, nodes need to be configured to use the same distribution
  # protocol (`-proto_dist`). We configure the Fly.io node to use IPv6
  # distribution (`-proto_dist inet6_tcp`). However, depending whether
  # the local node runs IPv4 or IPv6 distribution, we configure the
  # flyctl proxy to bind to a IPv4 or IPv6 loopback respectively. The
  # proxy always communicates with the Fly.io machine over IPv6, as
  # is the case with all internal networking. Consequently, regardless
  # of the protocol used by the local node, the remote node perceives
  # it as IPv6.
  #
  # Sidenote, a node using IPv6 distribution may accept connections
  # from a node using IPv4, depending on the `:kernel` application
  # configuration `inet_dist_listen_options` -> `ipv6_v6only`, which
  # has OS-specific value. However, we don't rely on this here.

  defstruct [:config, :node, :server_pid]

  use GenServer, restart: :temporary

  require Logger

  @type t :: %__MODULE__{
          config: config(),
          node: node() | nil,
          server_pid: pid() | nil
        }

  @type config :: %{
          token: String.t(),
          app_name: String.t(),
          region: String.t(),
          cpu_kind: String.t(),
          cpus: pos_integer(),
          memory_gb: pos_integer(),
          gpu_kind: String.t() | nil,
          gpus: pos_integer() | nil,
          volume_id: String.t() | nil,
          docker_tag: String.t()
        }

  @doc """
  Returns a new runtime instance.
  """
  @spec new(config()) :: t()
  def new(config) do
    %__MODULE__{config: config}
  end

  def __connect__(runtime) do
    {:ok, pid} =
      DynamicSupervisor.start_child(Livebook.RuntimeSupervisor, {__MODULE__, {runtime, self()}})

    pid
  end

  @doc false
  def start_link({runtime, caller}) do
    GenServer.start_link(__MODULE__, {runtime, caller})
  end

  @impl true
  def init({runtime, caller}) do
    state = %{primary_ref: nil, proxy_port: nil}
    {:ok, state, {:continue, {:init, runtime, caller}}}
  end

  @impl true
  def handle_continue({:init, runtime, caller}, state) do
    config = runtime.config
    local_port = get_free_port!()
    remote_port = 44444
    node_base = "remote_runtime_#{local_port}"

    runtime_data =
      %{
        node_base: node_base,
        cookie: Node.get_cookie(),
        dist_port: remote_port
      }
      |> :erlang.term_to_binary()
      |> Base.encode64()

    with {:ok, machine_id, machine_ip} <-
           with_log(caller, "create machine", fn ->
             create_machine(config, runtime_data)
           end),
         child_node <- :"#{node_base}@#{machine_id}.vm.#{config.app_name}.internal",
         {:ok, proxy_port} <-
           with_log(caller, "start proxy", fn ->
             start_fly_proxy(config.app_name, machine_ip, local_port, remote_port, config.token)
           end),
         :ok <-
           with_log(caller, "machine starting", fn ->
             await_machine_started(config, machine_id)
           end),
         :ok <-
           with_log(caller, "connect to node", fn ->
             connect_loop(child_node, 40, 250)
           end),
         {:ok, primary_pid} <- fetch_runtime_info(child_node) do
      primary_ref = Process.monitor(primary_pid)

      server_pid =
        with_log(caller, "initialize node", fn ->
          initialize_node(child_node)
        end)

      send(primary_pid, :node_initialized)

      runtime = %{runtime | node: child_node, server_pid: server_pid}
      send(caller, {:runtime_connect_done, self(), {:ok, runtime}})

      {:noreply, %{state | primary_ref: primary_ref, proxy_port: proxy_port}}
    else
      {:error, error} ->
        send(caller, {:runtime_connect_done, self(), {:error, error}})

        {:stop, :shutdown, state}
    end
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _pid, _reason}, state) when ref == state.primary_ref do
    {:stop, :shutdown, state}
  end

  def handle_info({port, _message}, state) when state.proxy_port == port do
    {:noreply, state}
  end

  defp create_machine(config, runtime_data) do
    base_image = Enum.find(Livebook.Config.docker_images(), &(&1.tag == config.docker_tag))
    image = "ghcr.io/livebook-dev/livebook:#{base_image.tag}"

    env =
      Map.merge(
        Map.new(base_image.env),
        %{
          "LIVEBOOK_RUNTIME" => runtime_data,
          "ERL_AFLAGS" => "-proto_dist inet6_tcp"
        }
      )

    name = "#{config.app_name}-livebook-runtime-#{Livebook.Utils.random_id()}"

    machine_config = %{
      image: image,
      guest: %{
        cpu_kind: config.cpu_kind,
        cpus: config.cpus,
        memory_mb: config.memory_gb * 1024,
        gpu_kind: config.gpu_kind,
        gpus: config.gpus
      },
      mounts: config.volume_id && [%{volume: config.volume_id, path: "/home/livebook"}],
      auto_destroy: true,
      restart: %{policy: "no"},
      env: env
    }

    case Livebook.FlyAPI.create_machine(
           config.token,
           config.app_name,
           name,
           config.region,
           machine_config
         ) do
      {:ok, %{id: machine_id, private_ip: machine_ip}} ->
        {:ok, machine_id, machine_ip}

      {:error, %{message: message}} ->
        {:error, "could not create machine, reason: #{message}"}
    end
  end

  defp await_machine_started(config, machine_id) do
    case Livebook.FlyAPI.await_machine_started(config.token, config.app_name, machine_id) do
      :ok ->
        :ok

      {:error, %{status: 408}} ->
        {:error,
         "timed out while waiting for the machine to start. See the app" <>
           " logs in the Fly.io dashboard to determine the reason"}

      {:error, %{message: message}} ->
        {:error, "failed while waiting for the machine to started, reason: #{message}"}
    end
  end

  defp connect_loop(_node, 0, _interval) do
    {:error, "could not establish connection with the node"}
  end

  defp connect_loop(node, attempts, interval) do
    if Node.connect(node) do
      :ok
    else
      Process.sleep(interval)
      connect_loop(node, attempts - 1, interval)
    end
  end

  defp get_free_port!() do
    {:ok, socket} = :gen_tcp.listen(0, active: false, reuseaddr: true)
    {:ok, port} = :inet.port(socket)
    :gen_tcp.close(socket)
    port
  end

  defp start_fly_proxy(app_name, host, local_port, remote_port, token) do
    with {:ok, flyctl_path} <- find_fly_executable() do
      ports = "#{local_port}:#{remote_port}"

      # We want the proxy to accept the same protocol that we are
      # going to use for distribution
      bind_addr =
        if Livebook.Utils.proto_dist() == :inet6_tcp do
          "[::1]"
        else
          "127.0.0.1"
        end

      args = [
        "proxy",
        ports,
        host,
        "--app",
        app_name,
        "--bind-addr",
        bind_addr,
        "--access-token",
        token,
        "--watch-stdin"
      ]

      port =
        Port.open(
          {:spawn_executable, flyctl_path},
          [:binary, :hide, :stderr_to_stdout, args: args]
        )

      port_ref = Port.monitor(port)

      result =
        receive do
          {^port, {:data, "Proxying " <> _}} ->
            {:ok, port}

          {^port, {:data, "Error: unknown flag: --watch-stdin\n"}} ->
            {:error,
             "failed to open fly proxy, because the current version " <>
               "is missing a required feature. Please update flyctl"}

          {^port, {:data, "Error: " <> error}} ->
            {:error, "failed to open fly proxy. Error: #{String.trim(error)}"}

          {:DOWN, ^port_ref, :port, _object, reason} ->
            {:error, "failed to open fly proxy. Process terminated, reason: #{inspect(reason)}"}
        after
          30_000 ->
            {:error, "failed to open fly proxy. Timed out after 30s"}
        end

      Port.demonitor(port_ref, [:flush])

      result
    end
  end

  defp find_fly_executable() do
    if path = System.find_executable("flyctl") do
      {:ok, path}
    else
      {:error,
       "no flyctl executable found in PATH. For installation instructions" <>
         " refer to https://fly.io/docs/flyctl/install"}
    end
  end

  defp fetch_runtime_info(child_node) do
    # Note: it is Livebook that starts the runtime node, so we know
    # that the node runs Livebook release of the exact same version
    #
    # Also, the remote node already has all the runtime modules in
    # the code path, compiled for its Elixir version, so we don't
    # need to check for matching Elixir version.

    %{pid: pid} = :erpc.call(child_node, :persistent_term, :get, [:livebook_runtime_info])

    {:ok, pid}
  end

  defp initialize_node(child_node) do
    init_opts = [
      runtime_server_opts: [
        extra_smart_cell_definitions: Livebook.Runtime.Definitions.smart_cell_definitions()
      ]
    ]

    Livebook.Runtime.ErlDist.initialize(child_node, init_opts)
  end

  defp with_log(caller, name, fun) do
    send(caller, {:runtime_connect_info, self(), name})

    {microseconds, result} = :timer.tc(fun)
    milliseconds = div(microseconds, 1000)

    case result do
      {:error, error} ->
        Logger.debug("[fly runtime] #{name} FAILED in #{milliseconds}ms, error: #{error}")

      _ ->
        Logger.debug("[fly runtime] #{name} finished in #{milliseconds}ms")
    end

    result
  end
end

defimpl Livebook.Runtime, for: Livebook.Runtime.Fly do
  alias Livebook.Runtime.ErlDist.RuntimeServer

  def describe(%{config: config} = runtime) do
    specs =
      [
        "#{config.cpus} #{config.cpu_kind} CPU",
        "#{config.memory_gb} GB RAM",
        config.gpu_kind && "#{config.gpus} #{config.gpu_kind} GPU"
      ]
      |> Enum.reject(&is_nil/1)
      |> Enum.join(", ")

    [
      {"Type", "Fly.io machine"},
      {"App", config.app_name},
      {"Specs", specs}
    ] ++
      if runtime.node do
        [{"Node name", Atom.to_string(runtime.node)}]
      else
        []
      end
  end

  def connect(runtime) do
    Livebook.Runtime.Fly.__connect__(runtime)
  end

  def take_ownership(runtime, opts \\ []) do
    RuntimeServer.attach(runtime.server_pid, self(), opts)
    Process.monitor(runtime.server_pid)
  end

  def disconnect(runtime) do
    :ok = RuntimeServer.stop(runtime.server_pid)
  end

  def duplicate(runtime) do
    Livebook.Runtime.Fly.new(runtime.config)
  end

  def evaluate_code(runtime, language, code, locator, parent_locators, opts \\ []) do
    RuntimeServer.evaluate_code(
      runtime.server_pid,
      language,
      code,
      locator,
      parent_locators,
      opts
    )
  end

  def forget_evaluation(runtime, locator) do
    RuntimeServer.forget_evaluation(runtime.server_pid, locator)
  end

  def drop_container(runtime, container_ref) do
    RuntimeServer.drop_container(runtime.server_pid, container_ref)
  end

  def handle_intellisense(runtime, send_to, request, parent_locators, node) do
    RuntimeServer.handle_intellisense(runtime.server_pid, send_to, request, parent_locators, node)
  end

  def read_file(runtime, path) do
    RuntimeServer.read_file(runtime.server_pid, path)
  end

  def transfer_file(runtime, path, file_id, callback) do
    RuntimeServer.transfer_file(runtime.server_pid, path, file_id, callback)
  end

  def relabel_file(runtime, file_id, new_file_id) do
    RuntimeServer.relabel_file(runtime.server_pid, file_id, new_file_id)
  end

  def revoke_file(runtime, file_id) do
    RuntimeServer.revoke_file(runtime.server_pid, file_id)
  end

  def start_smart_cell(runtime, kind, ref, attrs, parent_locators) do
    RuntimeServer.start_smart_cell(runtime.server_pid, kind, ref, attrs, parent_locators)
  end

  def set_smart_cell_parent_locators(runtime, ref, parent_locators) do
    RuntimeServer.set_smart_cell_parent_locators(runtime.server_pid, ref, parent_locators)
  end

  def stop_smart_cell(runtime, ref) do
    RuntimeServer.stop_smart_cell(runtime.server_pid, ref)
  end

  def fixed_dependencies?(_runtime), do: false

  def add_dependencies(_runtime, code, dependencies) do
    Livebook.Runtime.Dependencies.add_dependencies(code, dependencies)
  end

  def has_dependencies?(runtime, dependencies) do
    RuntimeServer.has_dependencies?(runtime.server_pid, dependencies)
  end

  def snippet_definitions(_runtime) do
    Livebook.Runtime.Definitions.snippet_definitions()
  end

  def search_packages(_runtime, send_to, search) do
    Livebook.Runtime.Dependencies.search_packages_on_hex(send_to, search)
  end

  def put_system_envs(runtime, envs) do
    RuntimeServer.put_system_envs(runtime.server_pid, envs)
  end

  def delete_system_envs(runtime, names) do
    RuntimeServer.delete_system_envs(runtime.server_pid, names)
  end

  def restore_transient_state(runtime, transient_state) do
    RuntimeServer.restore_transient_state(runtime.server_pid, transient_state)
  end

  def register_clients(runtime, clients) do
    RuntimeServer.register_clients(runtime.server_pid, clients)
  end

  def unregister_clients(runtime, client_ids) do
    RuntimeServer.unregister_clients(runtime.server_pid, client_ids)
  end

  def fetch_proxy_handler_spec(runtime) do
    RuntimeServer.fetch_proxy_handler_spec(runtime.server_pid)
  end

  def disconnect_node(runtime, node) do
    RuntimeServer.disconnect_node(runtime.server_pid, node)
  end
end

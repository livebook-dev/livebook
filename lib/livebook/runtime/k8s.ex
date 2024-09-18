defmodule Livebook.Runtime.K8s do
  # A runtime backed by a Kubernetes Pod managed by Livebook.
  #
  # This runtime uses the same concepts as the Fly runtime. In this
  # case, we start a Pod in a Kubernetes cluster and use kubectl to
  # proxy a local port to the distribution port of the remote node.
  # See `Livebook.Runtime.Fly` for more design details.

  defstruct [:config, :node, :req, :server_pid, :lv_pid, :pod_name]

  @type config :: %{
          context: String.t(),
          namespace: String.t(),
          home_pvc: String.t(),
          docker_tag: String.t(),
          pod_template: String.t()
        }

  @type t :: %__MODULE__{
          node: node() | nil,
          req: Req.Request.t(),
          server_pid: pid() | nil,
          lv_pid: pid(),
          pod_name: String.t() | nil
        }

  use GenServer, restart: :temporary

  require Logger

  alias Livebook.K8s.Pod

  @doc """
  Returns a new runtime instance.
  """
  @spec new(config :: map(), req :: Req.Request.t()) :: t()
  def new(config, req) do
    %__MODULE__{config: config, req: req, lv_pid: self()}
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
    state = %{primary_ref: nil}
    {:ok, state, {:continue, {:init, runtime, caller}}}
  end

  @impl true
  def handle_continue({:init, runtime, caller}, state) do
    config = runtime.config
    %{namespace: namespace, context: context} = config
    req = runtime.req

    kubeconfig =
      if System.get_env("KUBERNETES_SERVICE_HOST"),
        do: nil,
        else: System.get_env("KUBECONFIG") || Path.join(System.user_home!(), ".kube/config")

    cluster_data = get_cluster_data(kubeconfig)

    runtime_data =
      %{
        node_base: cluster_data.node_base,
        cookie: Node.get_cookie(),
        dist_port: cluster_data.remote_port
      }
      |> :erlang.term_to_binary()
      |> Base.encode64()

    parent = self()

    {:ok, watcher_pid} =
      DynamicSupervisor.start_child(
        Livebook.RuntimeSupervisor,
        {Task, fn -> watcher(parent, req, config) end}
      )

    with {:ok, pod_name} <-
           with_log(caller, "create pod", fn ->
             create_pod(req, config, runtime_data, cluster_data.remote_port)
           end),
         _ <- send(watcher_pid, {:pod_created, pod_name}),
         {:ok, pod_ip} <-
           with_pod_events(caller, "waiting for pod", req, namespace, pod_name, fn ->
             await_pod_ready(req, namespace, pod_name)
           end),
         child_node <- :"#{cluster_data.node_base}@#{pod_ip}",
         :ok <-
           with_log(caller, "start proxy", fn ->
             k8s_forward_port(
               kubeconfig,
               context,
               cluster_data,
               pod_name,
               namespace
             )
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

      send(watcher_pid, :done)

      runtime = %{runtime | node: child_node, server_pid: server_pid, pod_name: pod_name}
      send(caller, {:runtime_connect_done, self(), {:ok, runtime}})

      {:noreply, %{state | primary_ref: primary_ref}}
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

  def handle_info({port, _message}, state) when is_port(port) do
    {:noreply, state}
  end

  defp get_free_port!() do
    {:ok, socket} = :gen_tcp.listen(0, active: false, reuseaddr: true)
    {:ok, port} = :inet.port(socket)
    :gen_tcp.close(socket)
    port
  end

  defp with_log(caller, name, fun) do
    send(caller, {:runtime_connect_info, self(), name})

    {microseconds, result} = :timer.tc(fun)
    milliseconds = div(microseconds, 1000)

    case result do
      {:error, error} ->
        Logger.debug("[K8s runtime] #{name} FAILED in #{milliseconds}ms, error: #{error}")

      _ ->
        Logger.debug("[K8s runtime] #{name} finished in #{milliseconds}ms")
    end

    result
  end

  defp with_pod_events(caller, name, req, namespace, pod_name, fun) do
    with_log(caller, name, fn ->
      runtime_pid = self()

      event_watcher_pid =
        spawn_link(fn ->
          watch_result =
            req
            |> Req.merge(
              resource_path: "api/v1/namespaces/:namespace/events/:name",
              resource_list_path: "api/v1/namespaces/:namespace/events"
            )
            |> Kubereq.watch(namespace,
              field_selectors: [
                {"involvedObject.kind", "Pod"},
                {"involvedObject.name", pod_name}
              ]
            )

          case watch_result do
            {:ok, stream} ->
              Enum.each(stream, fn event ->
                message = Livebook.Utils.downcase_first(event["object"]["message"])
                Logger.debug(~s'[K8s runtime] Pod event: "#{message}"')
                send(caller, {:runtime_connect_info, runtime_pid, message})
              end)

            _error ->
              :ok
          end
        end)

      result = fun.()
      Process.exit(event_watcher_pid, :normal)
      result
    end)
  end

  defp watcher(parent, req, config) do
    ref = Process.monitor(parent)
    watcher_loop(%{ref: ref, config: config, req: req, pod_name: nil})
  end

  defp watcher_loop(state) do
    receive do
      {:DOWN, ref, :process, _pid, _reason} when ref == state.ref ->
        # If the parent process is killed, we try to eagerly free the
        # created resources
        if pod_name = state.pod_name do
          namespace = state.config.namespace
          _ = Kubereq.delete(state.req, namespace, pod_name)
        end

      {:pod_created, pod_name} ->
        watcher_loop(%{state | pod_name: pod_name})

      :done ->
        :ok
    end
  end

  defp create_pod(req, config, runtime_data, remote_port) do
    %{
      pod_template: pod_template,
      docker_tag: docker_tag,
      home_pvc: home_pvc,
      namespace: namespace
    } = config

    manifest =
      pod_template
      |> Pod.pod_from_template()
      |> Pod.add_env_vars([
        %{"name" => "LIVEBOOK_RUNTIME", "value" => runtime_data},
        %{
          "name" => "POD_IP",
          "valueFrom" => %{"fieldRef" => %{"apiVersion" => "v1", "fieldPath" => "status.podIP"}}
        },
        %{
          "name" => "POD_NAMESPACE",
          "valueFrom" => %{
            "fieldRef" => %{"apiVersion" => "v1", "fieldPath" => "metadata.namespace"}
          }
        },
        %{
          "name" => "POD_NAME",
          "valueFrom" => %{"fieldRef" => %{"apiVersion" => "v1", "fieldPath" => "metadata.name"}}
        }
      ])
      |> Pod.set_docker_tag(docker_tag)
      |> Pod.set_home_pvc(home_pvc)
      |> Pod.set_namespace(namespace)
      |> Pod.add_container_port(remote_port)

    case Kubereq.create(req, manifest) do
      {:ok, %{status: 201, body: %{"metadata" => %{"name" => pod_name}}}} ->
        {:ok, pod_name}

      {:ok, %{body: body}} ->
        {:error, "could not create Pod, reason: #{body["message"]}"}

      {:error, error} ->
        {:error, "could not create Pod, reason: #{Exception.message(error)}"}
    end
  end

  defp get_cluster_data(nil), do: %{node_base: "k8s_runtime", remote_port: 44444}

  defp get_cluster_data(_kubeconfig) do
    local_port = get_free_port!()
    %{node_base: "remote_runtime_#{local_port}", remote_port: 44444, local_port: local_port}
  end

  defp k8s_forward_port(nil, _, _, _, _), do: :ok

  defp k8s_forward_port(
         kubeconfig,
         context,
         %{local_port: local_port, remote_port: remote_port},
         pod_name,
         namespace
       ) do
    with {:ok, kubectl_path} <- find_kubectl_executable() do
      ports = "#{local_port}:#{remote_port}"

      # We want the proxy to accept the same protocol that we are
      # going to use for distribution
      bind_addr =
        if Livebook.Utils.proto_dist() == :inet6_tcp do
          "[::1]"
        else
          "127.0.0.1"
        end

      args =
        [
          "port-forward",
          "--kubeconfig",
          Path.expand(kubeconfig),
          "--context",
          context,
          "-n",
          namespace,
          pod_name,
          ports,
          "--address",
          bind_addr
        ]

      port =
        Port.open(
          {:spawn_executable, kubectl_path},
          [:binary, :hide, :stderr_to_stdout, args: args, env: []]
        )

      port_ref = Port.monitor(port)

      result =
        receive do
          {^port, {:data, "Forwarding from " <> _}} ->
            :ok

          {^port, {:data, "Error " <> _ = message}} ->
            {:error, "failed to port-forward. #{String.trim(message)}"}

          {:DOWN, ^port_ref, :port, _object, reason} ->
            {:error, "failed to port-forward. Process terminated, reason: #{inspect(reason)}"}
        after
          30_000 ->
            {:error, "failed to port-forward. Timed out after 30s"}
        end

      Port.demonitor(port_ref, [:flush])

      result
    end
  end

  defp find_kubectl_executable() do
    if path = System.find_executable("kubectl") do
      {:ok, path}
    else
      {:error, "no kubectl executable found in PATH."}
    end
  end

  defp await_pod_ready(req, namespace, pod_name) do
    with :ok <-
           Kubereq.wait_until(
             req,
             namespace,
             pod_name,
             fn
               :deleted ->
                 {:error, "The Pod was deleted before it started running."}

               pod ->
                 get_in(pod, [
                   "status",
                   "conditions",
                   Access.filter(&(&1["type"] == "Ready")),
                   "status"
                 ]) == ["True"]
             end,
             # 30 minutes
             1_800_000
           ),
         {:ok, %{status: 200, body: pod}} <- Kubereq.get(req, namespace, pod_name) do
      {:ok, pod["status"]["podIP"]}
    else
      {:error, :watch_timeout} ->
        {:error, "Timed out waiting for Pod to start up."}

      {:error, error} ->
        {:error, error}

      _other ->
        {:error, "Failed getting the Pod's IP address."}
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
end

defimpl Livebook.Runtime, for: Livebook.Runtime.K8s do
  alias Livebook.Runtime.ErlDist.RuntimeServer

  def describe(runtime) do
    [{"Type", "K8s Pod"}] ++
      if runtime.node do
        [{"Pod name", runtime.pod_name}, {"Node name", Atom.to_string(runtime.node)}]
      else
        []
      end
  end

  def connect(runtime) do
    Livebook.Runtime.K8s.__connect__(runtime)
  end

  def take_ownership(runtime, opts \\ []) do
    RuntimeServer.attach(runtime.server_pid, self(), opts)
    Process.monitor(runtime.server_pid)
  end

  def disconnect(runtime) do
    :ok = RuntimeServer.stop(runtime.server_pid)
  end

  def duplicate(runtime) do
    Livebook.Runtime.K8s.new(runtime.config, runtime.req)
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

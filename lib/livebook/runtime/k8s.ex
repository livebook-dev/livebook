defmodule Livebook.Runtime.K8s do
  # A runtime backed by a Kubernetes Pod managed by Livebook.
  #
  # This runtime uses the same concepts as the Fly runtime. In this
  # case, we start a Pod in a Kubernetes cluster and use kubectl to
  # proxy a local port to the distribution port of the remote node.
  # See `Livebook.Runtime.Fly` for more design details.

  defstruct [:config, :node, :server_pid, :lv_pid, :pod_name]

  use GenServer, restart: :temporary

  require Logger

  alias Livebook.Runtime.RemoteUtils
  alias Livebook.K8s.Pod

  @type t :: %__MODULE__{
          node: node() | nil,
          server_pid: pid() | nil,
          lv_pid: pid(),
          pod_name: String.t() | nil
        }

  @type config :: %{
          context: String.t(),
          namespace: String.t(),
          docker_tag: String.t(),
          pod_template: String.t(),
          pvc_name: String.t() | nil
        }

  @doc """
  Returns a new runtime instance.
  """
  @spec new(map()) :: t()
  def new(config) do
    %__MODULE__{config: config, lv_pid: self()}
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

    within_kubernetes? = System.get_env("KUBERNETES_SERVICE_HOST") != nil

    {node_base, local_port} =
      if within_kubernetes? do
        # When already running within Kubernetes we don't need the
        # proxy, the node is reachable directly
        {"k8s_runtime", nil}
      else
        local_port = RemoteUtils.get_free_port!()
        {"remote_runtime_#{local_port}", local_port}
      end

    kubeconfig =
      Kubereq.Kubeconfig.Default
      |> Kubereq.Kubeconfig.load()
      |> Kubereq.Kubeconfig.set_current_context(context)

    runtime_data = RemoteUtils.encode_runtime_data(node_base)

    parent = self()

    {:ok, watcher_pid} =
      DynamicSupervisor.start_child(
        Livebook.RuntimeSupervisor,
        {Task, fn -> watcher(parent, kubeconfig, config) end}
      )

    with {:ok, pod_name} <-
           with_log(caller, "create pod", fn ->
             create_pod(kubeconfig, config, runtime_data)
           end),
         _ <- send(watcher_pid, {:pod_created, pod_name}),
         {:ok, pod_ip} <-
           with_pod_events(caller, "waiting for pod", kubeconfig, namespace, pod_name, fn ->
             await_pod_ready(kubeconfig, namespace, pod_name)
           end),
         child_node <- :"#{node_base}@#{pod_ip}",
         :ok <-
           (if within_kubernetes? do
              :ok
            else
              with_log(caller, "start proxy", fn ->
                k8s_forward_port(context, local_port, pod_name, namespace)
              end)
            end),
         :ok <-
           with_log(caller, "connect to node", fn ->
             RemoteUtils.connect(child_node)
           end),
         %{pid: primary_pid} <- RemoteUtils.fetch_runtime_info(child_node) do
      primary_ref = Process.monitor(primary_pid)

      server_pid =
        with_log(caller, "initialize node", fn ->
          RemoteUtils.initialize_node(child_node)
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

  defp with_pod_events(caller, name, kubeconfig, namespace, pod_name, fun) do
    with_log(caller, name, fn ->
      runtime_pid = self()

      event_watcher_pid =
        spawn_link(fn ->
          with {:ok, stream} <- Livebook.K8sAPI.watch_pod_events(kubeconfig, namespace, pod_name) do
            for event <- stream do
              message = Livebook.Utils.downcase_first(event.message)
              send(caller, {:runtime_connect_info, runtime_pid, message})
              Logger.debug(~s/[k8s runtime] Pod event: "#{message}"/)
            end
          end
        end)

      result = fun.()
      Process.exit(event_watcher_pid, :normal)
      result
    end)
  end

  defp watcher(parent, kubeconfig, config) do
    ref = Process.monitor(parent)
    watcher_loop(%{ref: ref, config: config, kubeconfig: kubeconfig, pod_name: nil})
  end

  defp watcher_loop(state) do
    receive do
      {:DOWN, ref, :process, _pid, _reason} when ref == state.ref ->
        # If the parent process is killed, we try to eagerly free the
        # created resources
        if pod_name = state.pod_name do
          _ = Livebook.K8sAPI.delete_pod(state.kubeconfig, state.config.namespace, pod_name)
        end

      {:pod_created, pod_name} ->
        watcher_loop(%{state | pod_name: pod_name})

      :done ->
        :ok
    end
  end

  defp create_pod(kubeconfig, config, runtime_data) do
    %{
      pod_template: pod_template,
      docker_tag: docker_tag,
      pvc_name: pvc_name,
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
      |> Pod.set_namespace(namespace)
      |> Pod.add_container_port(RemoteUtils.remote_port())

    manifest =
      if pvc_name do
        Pod.set_pvc_name(manifest, pvc_name)
      else
        manifest
      end

    case Livebook.K8sAPI.create_pod(kubeconfig, manifest) do
      {:ok, %{name: name}} ->
        {:ok, name}

      {:error, %{message: message}} ->
        {:error, "could not create Pod, reason: #{message}"}
    end
  end

  defp k8s_forward_port(context, local_port, pod_name, namespace) do
    with {:ok, kubectl_path} <- find_kubectl_executable() do
      ports = "#{local_port}:#{RemoteUtils.remote_port()}"

      # We want the proxy to accept the same protocol that we are
      # going to use for distribution
      bind_addr =
        if Livebook.Utils.proto_dist() == :inet6_tcp do
          "[::1]"
        else
          "127.0.0.1"
        end

      kubeconfig = System.get_env("KUBECONFIG") || Path.join(System.user_home!(), ".kube/config")

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
      {:error, "no kubectl executable found in PATH"}
    end
  end

  defp await_pod_ready(kubeconfig, namespace, pod_name) do
    with :ok <- Livebook.K8sAPI.await_pod_ready(kubeconfig, namespace, pod_name),
         {:ok, %{ip: pod_ip}} <- Livebook.K8sAPI.get_pod(kubeconfig, namespace, pod_name) do
      {:ok, pod_ip}
    else
      {:error, %{message: message}} ->
        {:error, "failed while waiting for the Pod to start, reason: #{message}"}
    end
  end

  defp with_log(caller, name, fun) do
    send(caller, {:runtime_connect_info, self(), name})
    RemoteUtils.with_log("[k8s runtime] #{name}", fun)
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
    Livebook.Runtime.K8s.new(runtime.config)
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

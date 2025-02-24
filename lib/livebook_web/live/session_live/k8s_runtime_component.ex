defmodule LivebookWeb.SessionLive.K8sRuntimeComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  alias Livebook.K8s

  @kubeconfig_pipeline Application.compile_env(:livebook, :k8s_kubeconfig_pipeline)

  @impl true
  def mount(socket) do
    unless Livebook.Config.runtime_enabled?(Livebook.Runtime.K8s) do
      raise "runtime module not allowed"
    end

    kubeconfig = Kubereq.Kubeconfig.load(@kubeconfig_pipeline)
    context_options = Enum.map(kubeconfig.contexts, & &1["name"])

    {:ok,
     socket
     |> assign(
       kubeconfig: kubeconfig,
       context_options: context_options,
       context: nil,
       cluster_check: %{status: :initial, error: nil},
       namespace: nil,
       namespace_options: nil,
       rbac: %{status: :inflight, errors: [], permissions: []},
       pvcs: nil,
       pvc_action: nil,
       pvc_name: nil,
       docker_tag: hd(Livebook.Config.docker_images()).tag,
       pod_template: %{template: K8s.Pod.default_pod_template(), error_message: nil},
       save_config_payload: nil
     )}
  end

  @impl true
  def update(%{event: :open_save_config}, socket) do
    {:ok, assign(socket, save_config_payload: build_config(socket))}
  end

  def update(%{event: :close_save_config}, socket) do
    {:ok, assign(socket, save_config_payload: nil)}
  end

  def update(%{event: {:load_config, config_defaults}}, socket) do
    {:ok,
     socket
     |> assign(config_defaults: config_defaults)
     |> load_config_defaults()}
  end

  @impl true
  @spec update(maybe_improper_list() | map(), any()) :: {:ok, any()}
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    socket =
      cond do
        is_map_key(socket.assigns, :config_defaults) ->
          socket

        is_struct(assigns.runtime, Livebook.Runtime.K8s) ->
          %{config: config} = assigns.runtime

          config_defaults =
            Map.new(config, fn {key, value} ->
              {Atom.to_string(key), value}
            end)

          socket
          |> assign(config_defaults: config_defaults)
          |> load_config_defaults()

        true ->
          socket
          |> assign(config_defaults: nil)
          |> set_context(socket.assigns.kubeconfig.current_context)
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <div :if={warning = kubectl_warning()} class="mb-2">
        <.message_box kind="warning" message={warning} />
      </div>

      <p class="text-gray-700">
        Start a temporary Kubernetes Pod with an Elixir node to evaluate code.
        The Pod is automatically deleted, once you disconnect the runtime.
      </p>

      <.live_component
        module={LivebookWeb.SessionLive.SaveRuntimeConfigComponent}
        id="save-runtime-config"
        hub={@hub}
        hub_secrets={@hub_secrets}
        target={{__MODULE__, @id}}
        save_config_payload={@save_config_payload}
        secret_prefix="K8S_RUNTIME_"
      />

      <div :if={@save_config_payload == nil}>
        <.message_box :if={@kubeconfig.current_cluster == nil} kind="error">
          In order to use the Kubernetes context, you need to set the <code>KUBECONFIG</code>
          environment variable to a path pointing to a <a
            class="text-blue-600 hover:text-blue-700"
            href="https://kubernetes.io/docs/reference/config-api/kubeconfig.v1/"
            phx-no-format
          >Kubernetes configuration</a> YAML file (e.g. to <code>"~/.kube/config"</code>).
        </.message_box>

        <form
          :if={@context_options != []}
          phx-change="set_context"
          phx-nosubmit
          phx-target={@myself}
          class="mt-1 flex flex-col gap-4"
        >
          <.select_field name="context" value={@context} label="Context" options={@context_options} />
          <.loader :if={@cluster_check.status == :inflight} />
          <.cluster_check_error :if={@cluster_check.status == :error} error={@cluster_check.error} />
        </form>

        <form
          :if={@cluster_check.status == :ok}
          phx-change="set_namespace"
          phx-nosubmit
          phx-target={@myself}
          class="mt-4"
        >
          <.select_field
            :if={@namespace_options != nil}
            name="namespace"
            value={@namespace}
            label="Namespace"
            options={@namespace_options}
          />
          <div :if={@namespace_options == nil}>
            <.text_field name="namespace" value={@namespace} label="Namespace" phx-debounce="600" />
            <div class="text-sm text-amber-600">
              Authenticated user has no permission to list namespaces. But you can enter a name of an existing namespace.
            </div>
          </div>
        </form>

        <.message_box :if={@rbac.status == :errors} kind="error">
          <%= for error <- @rbac.errors do %>
            <.rbac_error error={error} />
          <% end %>
        </.message_box>

        <div :if={@rbac.status == :ok} class="mt-8">
          <div class="text-lg text-gray-800 font-semibold">
            Pod
          </div>
          <div class="mt-1 text-gray-700">
            You can fully customize the runtime pod by editing the pod template.
          </div>
          <form
            :if={@cluster_check.status == :ok}
            phx-change="set_docker_tag"
            phx-nosubmit
            phx-target={@myself}
            class="mt-4"
          >
            <.radio_field
              :if={@rbac.status == :ok}
              name="docker_tag"
              value={@docker_tag}
              label="Base Docker image"
              options={LivebookWeb.AppComponents.docker_tag_options()}
            />
          </form>
          <form
            :if={@cluster_check.status == :ok}
            phx-change="set_pod_template"
            phx-nosubmit
            phx-target={@myself}
            class="mt-4"
          >
            <.textarea_field
              name="pod_template"
              label="Template"
              value={@pod_template.template}
              phx-debounce={500}
              monospace={true}
              phx-hook="TextareaAutosize"
            />

            <.message_box
              :if={@pod_template.error_message}
              kind="error"
              message={@pod_template.error_message}
            />
          </form>
        </div>

        <.storage_config
          :if={@rbac.status == :ok}
          myself={@myself}
          pvc_name={@pvc_name}
          pvcs={@pvcs}
          pvc_action={@pvc_action}
          rbac={@rbac}
        />

        <div :if={@rbac.status == :ok} class="mt-8">
          <div class="flex gap-2">
            <.button phx-click="init" phx-target={@myself} disabled={@runtime_status == :connecting}>
              {label(@namespace, @runtime, @runtime_status)}
            </.button>
            <.button
              :if={@runtime_status == :connecting}
              color="red"
              outlined
              phx-click="disconnect"
              phx-target={@myself}
            >
              Disconnect
            </.button>
          </div>
          <div
            :if={reconnecting?(@namespace, @runtime) && @runtime_connect_info}
            class="mt-4 scroll-mb-8"
            phx-mounted={JS.dispatch("lb:scroll_into_view", detail: %{behavior: "instant"})}
          >
            <.message_box kind="info">
              <div class="flex items-center gap-2">
                <.spinner />
                <span>Step: {@runtime_connect_info}</span>
              </div>
            </.message_box>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp storage_config(assigns) do
    ~H"""
    <div class="mt-8">
      <div class="text-lg text-gray-800 font-semibold">
        Storage
      </div>
      <div class="mt-1 text-gray-700">
        Every time you connect to the runtime, a fresh machine is created.
        In order to persist data and caches, you can optionally mount a
        volume at <code>/home/livebook</code>. Setting a Persistent Volume
        Claim will automatically add a <code>.template.spec.volumes[]</code>
        entry and a <code>.template.spec.containers[name="livebook-runtime"].volumeMounts[]</code>
        entry to the pod template.
      </div>

      <div class="mt-4 flex flex-col">
        <div class="flex items-start gap-1">
          <form phx-change="set_pvc_name" phx-nosubmit phx-target={@myself} class="grow">
            <.select_field
              :if={@rbac.permissions.list_pvc}
              value={@pvc_name}
              name="pvc_name"
              label="Persistent Volume Claim"
              options={[{"None", nil} | @pvcs]}
            />
            <div :if={!@rbac.permissions.list_pvc}>
              <.text_field value={@pvc_name} name="pvc_name" label="Persistent Volume Claim" />
              <div class="text-sm text-amber-600">
                Authenticated user has no permission to list PVCs. But you can enter a name of an existing PVC to be attached.
              </div>
            </div>
          </form>

          <div class="mt-7 flex items-center gap-1">
            <span
              :if={@rbac.permissions.delete_pvc}
              class="tooltip left"
              data-tooltip="Delete selected PVC"
            >
              <.icon_button
                phx-click="delete_pvc"
                phx-target={@myself}
                disabled={@pvc_name == nil or @pvc_action != nil}
              >
                <.remix_icon icon="delete-bin-6-line" />
              </.icon_button>
            </span>
            <span
              :if={@rbac.permissions.create_pvc}
              class="tooltip left"
              data-tooltip="Create new PVC"
            >
              <.icon_button phx-click="new_pvc" phx-target={@myself}>
                <.remix_icon icon="add-line" />
              </.icon_button>
            </span>
          </div>
        </div>
        <div
          :if={@pvc_action[:type] == :delete}
          class="px-4 py-3 mt-4 flex space-x-4 items-center border border-gray-200 rounded-lg"
        >
          <p class="grow text-gray-700 text-sm">
            Are you sure you want to irreversibly delete Persistent Volume Claim <span class="font-semibold">{@pvc_name}</span>?
          </p>
          <div class="flex space-x-4">
            <button
              class="text-red-600 font-medium text-sm whitespace-nowrap"
              phx-click="confirm_delete_pvc"
              phx-target={@myself}
              disabled={@pvc_action.status == :inflight}
            >
              <.remix_icon icon="delete-bin-6-line" class="align-middle mr-1" />
              {if @pvc_action[:type] == :delete, do: "Delete", else: "Deleting..."}
            </button>
            <button
              class="text-gray-600 font-medium text-sm"
              phx-click="cancel_delete_pvc"
              phx-target={@myself}
              disabled={@pvc_action.status == :inflight}
            >
              Cancel
            </button>
          </div>
        </div>

        <.form
          :let={pvcf}
          :if={@pvc_action[:type] == :new}
          for={@pvc_action.changeset}
          as={:pvc}
          phx-submit="create_pvc"
          phx-change="validate_pvc"
          phx-target={@myself}
          class="flex gap-2 mt-4 items-center"
          autocomplete="off"
          spellcheck="false"
        >
          <div>
            <.remix_icon icon="corner-down-right-line" class="text-gray-400 text-lg" />
          </div>
          <div class="grid grid-cols-4 gap-2 grow">
            <.text_field field={pvcf[:name]} placeholder="Name" />
            <.text_field field={pvcf[:size_gb]} placeholder="Size (Gi)" type="number" min="1" />
            <.select_field
              field={pvcf[:access_mode]}
              options={["ReadWriteOnce", "ReadWriteMany", "ReadWriteOncePod"]}
            />
            <.select_field field={pvcf[:storage_class]} options={@pvc_action.storage_classes} />
          </div>
          <.button
            type="submit"
            disabled={not @pvc_action.changeset.valid? or @pvc_action.status == :inflight}
          >
            {if(@pvc_action.status == :inflight, do: "Creating...", else: "Create")}
          </.button>
          <.button
            type="button"
            color="gray"
            outlined
            phx-click="cancel_new_pvc"
            phx-target={@myself}
            disabled={@pvc_action.status == :inflight}
          >
            Cancel
          </.button>
        </.form>
        <.message_box
          :if={@pvc_action[:status] == :error}
          kind="error"
          message={"Error: " <> @pvc_action.error.message}
        />
      </div>
    </div>
    """
  end

  defp loader(assigns) do
    ~H"""
    <div class="flex items-center gap-2">
      <span class="text-sm font-gray-700">Loading</span>
      <.spinner />
    </div>
    """
  end

  defp cluster_check_error(%{error: %{status: 401}} = assigns) do
    ~H"""
    <.message_box kind="error" message="Authentication with cluster failed." />
    """
  end

  defp cluster_check_error(assigns) do
    ~H"""
    <.message_box kind="error" message={"Connection to cluster failed, reason: " <> @error.message} />
    """
  end

  defp rbac_error(%{error: {:ok, access_review}} = assigns) do
    namespace = access_review.namespace
    verb = access_review.verb

    path =
      String.trim(
        "#{access_review.group}/#{access_review.version}/#{access_review.resource}",
        "/"
      )

    assigns = assign(assigns, verb: verb, path: path, namespace: namespace)

    ~H"""
    <div class="flex items-center justify-between">
      <div>
        Authenticated user has no permission to <span class="font-semibold">{@verb}</span>
        <code>{@path}</code>
        <span :if={@namespace}> in namespace <code>{@namespace}</code> (or the namespace doesn't exist)</span>.
      </div>
    </div>
    """
  end

  defp rbac_error(%{error: {:error, %{message: message}}} = assigns) do
    assigns = assign(assigns, :message, message)

    ~H"""
    <div>{@message}</div>
    """
  end

  @impl true
  def handle_event("set_context", %{"context" => context}, socket) do
    {:noreply, socket |> set_context(context) |> set_namespace(nil)}
  end

  def handle_event("set_namespace", %{"namespace" => namespace}, socket) do
    {:noreply, set_namespace(socket, namespace)}
  end

  def handle_event("set_docker_tag", %{"docker_tag" => docker_tag}, socket) do
    {:noreply, assign(socket, :docker_tag, docker_tag)}
  end

  def handle_event("set_pod_template", %{"pod_template" => pod_template}, socket) do
    {:noreply, set_pod_template(socket, pod_template)}
  end

  def handle_event("set_pvc_name", %{"pvc_name" => pvc_name}, socket) do
    {:noreply, assign(socket, :pvc_name, pvc_name)}
  end

  def handle_event("new_pvc", %{}, socket) do
    pvc_action = %{
      type: :new,
      changeset: K8s.PVC.changeset(),
      storage_classes: storage_classes(socket.assigns),
      status: :initial,
      error: nil
    }

    {:noreply, assign(socket, pvc_action: pvc_action)}
  end

  def handle_event("validate_pvc", %{"pvc" => pvc}, socket) do
    changeset =
      pvc
      |> K8s.PVC.changeset()
      |> Map.replace!(:action, :validate)

    {:noreply, assign_nested(socket, :pvc_action, changeset: changeset)}
  end

  def handle_event("cancel_new_pvc", %{}, socket) do
    {:noreply, assign(socket, pvc_action: nil)}
  end

  def handle_event("create_pvc", %{"pvc" => pvc}, socket) do
    pvc
    |> K8s.PVC.changeset()
    |> apply_action(:insert)
    |> case do
      {:ok, applied_pvc} ->
        {:noreply, create_pvc(socket, applied_pvc)}

      {:error, changeset} ->
        {:noreply, assign_nested(socket, :pvc_action, changeset: changeset)}
    end
  end

  def handle_event("delete_pvc", %{}, socket) do
    pvc_action = %{type: :delete, status: :initial, error: nil}
    {:noreply, assign(socket, pvc_action: pvc_action)}
  end

  def handle_event("confirm_delete_pvc", %{}, socket) do
    %{namespace: namespace, pvc_name: name} = socket.assigns
    kubeconfig = socket.assigns.kubeconfig

    socket =
      socket
      |> start_async(:delete_pvc, fn ->
        Livebook.K8sAPI.delete_pvc(kubeconfig, namespace, name)
      end)
      |> assign_nested(:pvc_action, status: :inflight)

    {:noreply, socket}
  end

  def handle_event("cancel_delete_pvc", %{}, socket) do
    {:noreply, assign(socket, pvc_action: nil)}
  end

  def handle_event("init", %{}, socket) do
    config = build_config(socket)
    runtime = Livebook.Runtime.K8s.new(config)
    Livebook.Session.set_runtime(socket.assigns.session.pid, runtime)
    Livebook.Session.connect_runtime(socket.assigns.session.pid)
    {:noreply, socket}
  end

  def handle_event("disconnect", %{}, socket) do
    Livebook.Session.disconnect_runtime(socket.assigns.session.pid)
    {:noreply, socket}
  end

  @impl true
  def handle_async(:rbac_check, {:ok, %{errors: errors, permissions: permissions}}, socket) do
    status = if errors == [], do: :ok, else: :errors
    {:noreply, assign(socket, :rbac, %{status: status, errors: errors, permissions: permissions})}
  end

  def handle_async(:cluster_check, {:ok, results}, socket) do
    [access_review_result, namespaces_result] = results

    access_review_result =
      case access_review_result do
        {:ok, %{allowed: true}} -> :ok
        {:ok, %{allowed: false}} -> {:error, %{message: "no access", status: nil}}
        error -> error
      end

    namespaces_result =
      case namespaces_result do
        {:ok, namespaces} ->
          namespace_options = Enum.map(namespaces, & &1.name)

          default_namespace =
            if socket.assigns.context == socket.assigns.config_defaults["context"] do
              socket.assigns.config_defaults["namespace"]
            end

          context_namespace = socket.assigns.kubeconfig.current_namespace
          namespace = default_namespace || context_namespace || List.first(namespace_options)
          {:ok, namespace_options, namespace}

        {:error, %{status: 403}} ->
          # No access to list namespaces, we will show an input instead
          {:ok, nil, nil}

        other ->
          other
      end

    socket =
      with :ok <- access_review_result,
           {:ok, namespace_options, namespace} <- namespaces_result do
        socket
        |> assign(:namespace_options, namespace_options)
        |> set_namespace(namespace)
        |> assign(:cluster_check, %{status: :ok, error: nil})
      else
        {:error, error} ->
          socket
          |> assign(:namespace_options, nil)
          |> assign(:cluster_check, %{status: :error, error: error})
      end

    {:noreply, socket}
  end

  def handle_async(:delete_pvc, {:ok, result}, socket) do
    socket =
      case result do
        :ok ->
          socket
          |> assign(pvc_name: nil, pvc_action: nil)
          |> pvc_options()

        {:error, error} ->
          assign_nested(socket, :pvc_action, status: :error, error: error)
      end

    {:noreply, socket}
  end

  def handle_async(:create_pvc, {:ok, result}, socket) do
    socket =
      case result do
        {:ok, %{name: name}} ->
          socket
          |> assign(pvc_name: name, pvc_action: nil)
          |> pvc_options()

        {:error, error} ->
          assign_nested(socket, :pvc_action, status: :error, error: error)
      end

    {:noreply, socket}
  end

  defp label(namespace, runtime, runtime_status) do
    reconnecting? = reconnecting?(namespace, runtime)

    case {reconnecting?, runtime_status} do
      {true, :connected} -> "Reconnect"
      {true, :connecting} -> "Connecting..."
      _ -> "Connect"
    end
  end

  defp reconnecting?(namespace, runtime) do
    match?(%Livebook.Runtime.K8s{config: %{namespace: ^namespace}}, runtime)
  end

  defp create_pvc(socket, pvc) do
    namespace = socket.assigns.namespace
    manifest = K8s.PVC.manifest(pvc, namespace)
    kubeconfig = socket.assigns.kubeconfig

    socket
    |> start_async(:create_pvc, fn -> Livebook.K8sAPI.create_pvc(kubeconfig, manifest) end)
    |> assign_nested(:pvc_action, status: :inflight)
  end

  defp set_context(socket, nil), do: assign(socket, :context, nil)

  defp set_context(socket, context) do
    kubeconfig = Kubereq.Kubeconfig.set_current_context(socket.assigns.kubeconfig, context)

    socket
    |> start_async(:cluster_check, fn ->
      [
        Task.async(fn ->
          Livebook.K8sAPI.create_access_review(kubeconfig,
            verb: "create",
            group: "authorization.k8s.io",
            version: "v1",
            resource: "selfsubjectaccessreviews"
          )
        end),
        Task.async(fn -> Livebook.K8sAPI.list_namespaces(kubeconfig) end)
      ]
      |> Task.await_many(:infinity)
    end)
    |> assign(
      kubeconfig: kubeconfig,
      context: context,
      namespace: nil,
      namespace_options: nil,
      cluster_check: %{status: :inflight, error: nil}
    )
  end

  defp set_namespace(socket, nil) do
    assign(socket, namespace: nil, rbac: %{status: :inflight, errors: [], permissions: []})
  end

  defp set_namespace(socket, ns) do
    kubeconfig = socket.assigns.kubeconfig

    socket
    |> start_async(:rbac_check, fn ->
      resource_attribute_list = [
        # Required permissions
        [verb: "get", version: "v1", resource: "pods", namespace: ns],
        [verb: "list", version: "v1", resource: "pods", namespace: ns],
        [verb: "watch", version: "v1", resource: "pods", namespace: ns],
        [verb: "create", version: "v1", resource: "pods", namespace: ns],
        [verb: "delete", version: "v1", resource: "pods", namespace: ns],
        [verb: "create", version: "v1", resource: "pods/portforward", namespace: ns],
        # Optional permissions
        [verb: "list", version: "v1", resource: "persistentvolumeclaims", namespace: ns],
        [verb: "create", version: "v1", resource: "persistentvolumeclaims", namespace: ns],
        [verb: "delete", version: "v1", resource: "persistentvolumeclaims", namespace: ns],
        [verb: "list", version: "v1", resource: "storageclasses", namespace: ns]
      ]

      {required_permissions, optional_permissions} =
        resource_attribute_list
        |> Enum.map(&Task.async(fn -> Livebook.K8sAPI.create_access_review(kubeconfig, &1) end))
        |> Task.await_many(:infinity)
        |> Enum.split(6)

      errors =
        Enum.reject(required_permissions, &match?({:ok, %{allowed: true}}, &1))

      permissions =
        optional_permissions
        |> Enum.map(&match?({:ok, %{allowed: true}}, &1))
        |> then(&Enum.zip([:list_pvc, :create_pvc, :delete_pvc, :list_storage_classes], &1))
        |> Map.new()

      %{errors: errors, permissions: permissions}
    end)
    |> assign(
      namespace: ns,
      rbac: %{status: :inflight, errors: :inflight, permissions: :inflight}
    )
    |> pvc_options()
  end

  def set_pod_template(socket, pod_template_yaml) do
    namespace = socket.assigns.namespace

    with {:parse, {:ok, pod_template}} <-
           {:parse, YamlElixir.read_from_string(pod_template_yaml)},
         {:validate, :ok} <- {:validate, K8s.Pod.validate_pod_template(pod_template, namespace)} do
      assign(socket, :pod_template, %{template: pod_template_yaml, error_message: nil})
    else
      {:parse, {:error, error}} ->
        assign(socket, :pod_template, %{
          template: pod_template_yaml,
          error_message: Exception.message(error)
        })

      {:validate, {:error, message}} ->
        assign(socket, :pod_template, %{
          template: pod_template_yaml,
          error_message: message
        })
    end
  end

  defp pvc_options(%{assigns: %{rbac: %{permissions: %{list_pvc: false}}}} = socket) do
    assign(socket, :pvcs, [])
  end

  defp pvc_options(socket) do
    %{kubeconfig: kubeconfig, namespace: namespace} = socket.assigns

    case Livebook.K8sAPI.list_pvcs(kubeconfig, namespace) do
      {:ok, pvcs} ->
        pvcs = for pvc <- pvcs, pvc.deleted_at == nil, do: pvc.name
        assign(socket, :pvcs, pvcs)

      _other ->
        assign(socket, :pvcs, [])
    end
  end

  defp storage_classes(%{rbac: %{permissions: %{list_storage_classes: false}}}), do: []

  defp storage_classes(%{kubeconfig: kubeconfig}) do
    case Livebook.K8sAPI.list_storage_classes(kubeconfig) do
      {:ok, storage_classes} ->
        Enum.map(storage_classes, & &1.name)

      _other ->
        []
    end
  end

  defp load_config_defaults(socket) do
    config_defaults = socket.assigns.config_defaults

    socket
    |> assign(
      pvc_name: config_defaults["pvc_name"],
      docker_tag: config_defaults["docker_tag"]
    )
    |> set_pod_template(config_defaults["pod_template"])
    |> set_context(config_defaults["context"])
  end

  defp build_config(socket) do
    %{
      context: socket.assigns.context,
      namespace: socket.assigns.namespace,
      pvc_name: socket.assigns.pvc_name,
      docker_tag: socket.assigns.docker_tag,
      pod_template: socket.assigns.pod_template.template
    }
  end

  defp kubectl_warning() do
    if System.find_executable("kubectl") == nil do
      warning = "Could not find kubectl in PATH. Make sure to install it and add it to PATH."

      if Livebook.Config.app?() do
        windows? = match?({:win32, _}, :os.type())

        {path, command} =
          if windows? do
            path = "#{System.get_env("USERPROFILE", "%USERPROFILE%")}\\.livebookdesktop.bat"
            command = ~s|set "PATH=C:\\path\\to\\dir\\;%PATH%"|
            {path, command}
          else
            path = "#{System.get_env("HOME", "$HOME")}/.livebookdesktop.sh"
            command = ~s|export PATH="/path/to/dir:$PATH"|
            {path, command}
          end

        """
        #{warning}

        For Livebook Desktop, this can be done by creating a file at #{path} with:

            #{command}
        """
      else
        warning
      end
    end
  end
end

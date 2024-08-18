defmodule LivebookWeb.SessionLive.K8sRuntimeComponent do
  alias Livebook.K8s.{Pod, PVC}
  use LivebookWeb, :live_component

  import Ecto.Changeset

  alias Livebook.{K8s.Auth, Session, Runtime}

  @config_secret_prefix "K8S_RUNTIME_"

  @impl true
  def mount(socket) do
    unless Livebook.Config.runtime_enabled?(Livebook.Runtime.K8s) do
      raise "runtime module not allowed"
    end

    kubeconfig = Kubereq.Kubeconfig.load(Livebook.K8s.Kubeconfig)
    context_options = Enum.map(kubeconfig.contexts, & &1["name"])

    {:ok,
     socket
     |> assign(
       kubeconfig: kubeconfig,
       context_options: context_options,
       context: nil,
       reqs: nil,
       cluster_check: %{status: :initial, error: nil},
       namespace: nil,
       namespace_options: nil,
       rbac_permissions: nil,
       rbac_error: nil,
       basic_specs_changeset: Pod.BasicSpecs.changeset(),
       advanced_specs_changeset: Pod.AdvancedSpecs.changeset(),
       save_config: nil,
       pvcs: nil,
       pvc_action: nil,
       home_pvc: nil
     )
     |> set_context(kubeconfig.current_context)}
  end

  @impl true
  @spec update(maybe_improper_list() | map(), any()) :: {:ok, any()}
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    socket =
      cond do
        is_map_key(socket.assigns, :config_defaults) ->
          socket

        is_struct(assigns.runtime, Runtime.K8s) ->
          %{config: config} = assigns.runtime

          config_defaults =
            Map.new(config, fn {key, value} ->
              {Atom.to_string(key), value}
            end)

          socket
          |> assign(config_defaults: config_defaults)
          |> load_config_defaults()

        true ->
          assign(socket, config_defaults: nil)
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <p class="text-gray-700">
        Start a temporary Kubernetes Pod with an Elixir node to evaluate code.
        The Pod is automatically deleted, once you disconnect the runtime.
      </p>

      <.save_config_form :if={@save_config} save_config={@save_config} hub={@hub} myself={@myself} />

      <div :if={@save_config == nil}>
        <.config_actions hub_secrets={@hub_secrets} myself={@myself} />

        <.message_box :if={@kubeconfig.current_cluster == nil} kind={:error}>
          In order to use the Kubernetes context, you need to set the <code>KUBECONFIG</code>
          or <code>LIVEBOOK_KUBECONFIG</code>
          environment variable to a path pointing to a <a
            class="text-blue-600 hover:text-blue-700"
            href="https://kubernetes.io/docs/reference/config-api/kubeconfig.v1/"
            phx-no-format
          >Kubernetes configuration</a> YAML file (e.g. to <code>"~/.kube/config"</code>).
        </.message_box>

        <.form
          :let={f}
          :if={@context_options != []}
          phx-change="set_context"
          phx-nosubmit
          phx-target={@myself}
          class="mt-4"
        >
          <.select_field
            field={f[:context]}
            label="Context"
            options={@context_options}
            value={@context}
          />
        </.form>

        <.loader :if={@cluster_check.status == :inflight} />

        <.cluster_check_error :if={@cluster_check.status == :error} error={@cluster_check.error} />
        <.form
          :let={f}
          :if={@cluster_check.status == :ok}
          phx-change="set_namespace"
          phx-nosubmit
          phx-target={@myself}
          class="mt-4"
        >
          <.select_field
            :if={@namespace_options != nil}
            field={f[:namespace]}
            label="Namespace"
            options={@namespace_options}
            value={@namespace}
          />
          <.text_field
            :if={@namespace_options == nil}
            field={f[:namespace]}
            value={@namespace}
            label="Namespace"
            phx-debounce="600"
          />
        </.form>

        <.rbac_error :if={@rbac_error} error={@rbac_error} />

        <.basic_specs_config
          :if={@rbac_permissions}
          basic_specs_changeset={@basic_specs_changeset}
          myself={@myself}
        />
        <.storage_config
          :if={@rbac_permissions}
          myself={@myself}
          home_pvc={@home_pvc}
          pvcs={@pvcs}
          pvc_action={@pvc_action}
          rbac_permissions={@rbac_permissions}
        />
        <.advanced_spec_config
          :if={@rbac_permissions}
          advanced_specs_changeset={@advanced_specs_changeset}
          myself={@myself}
        />

        <div class="mt-8">
          <.button
            phx-click="init"
            phx-target={@myself}
            disabled={
              @runtime_status == :connecting or not @basic_specs_changeset.valid? or
                not @advanced_specs_changeset.valid?
            }
          >
            Connect
          </.button>
          <%!-- <div
            :if={reconnecting?(@app_name, @runtime) && @runtime_connect_info}
            class="mt-4 scroll-mb-8"
            phx-mounted={JS.dispatch("lb:scroll_into_view", detail: %{behavior: "instant"})}
          >
            <.message_box kind={:info}>
              <div class="flex items-center gap-2">
                <.spinner />
                <span>Step: <%= @runtime_connect_info %></span>
              </div>
            </.message_box>
          </div> --%>
        </div>
      </div>
    </div>
    """
  end

  defp basic_specs_config(assigns) do
    ~H"""
    <div class="mt-8">
      <div class="text-lg text-gray-800 font-semibold">
        Specs
      </div>
      <.form
        :let={f}
        for={@basic_specs_changeset}
        as={:specs}
        class="mt-4 flex flex-col gap-4"
        phx-change="validate_basic_specs"
        phx-nosubmit
        phx-target={@myself}
        autocomplete="off"
        spellcheck="false"
      >
        <.radio_field
          field={f[:docker_tag]}
          label="Base Docker image"
          options={LivebookWeb.AppComponents.docker_tag_options()}
        />

        <div class="grid grid-cols-2 gap-6">
          <div>
            <.inputs_for :let={requests} field={f[:resource_requests]}>
              <div class="text-base text-gray-800 font-medium">
                Resource Requests
              </div>
              <div class="grid grid-cols-3 gap-2">
                <.text_field field={requests[:cpu]} label="CPUs" />
                <.text_field field={requests[:memory]} label="Memory" />
                <.text_field field={requests[:gpu]} label="GPUs" />
              </div>
            </.inputs_for>
          </div>
          <div>
            <.inputs_for :let={limits} field={f[:resource_limits]}>
              <div class="text-base text-gray-800 font-medium">
                Resource Limits
              </div>
              <div class="grid grid-cols-3 gap-2">
                <.text_field field={limits[:cpu]} label="CPUs" />
                <.text_field field={limits[:memory]} label="Memory" />
                <.text_field field={limits[:gpu]} label="GPUs" />
              </div>
            </.inputs_for>
          </div>
        </div>
      </.form>
    </div>
    """
  end

  defp advanced_spec_config(assigns) do
    ~H"""
    <div class="mt-8">
      <div class="text-lg text-gray-800 font-semibold">
        Advanced Specs
      </div>
      <.form
        :let={f}
        for={@advanced_specs_changeset}
        as={:specs}
        class="mt-4 flex flex-col gap-4"
        phx-change="validate_advanced_specs"
        phx-nosubmit
        phx-target={@myself}
        autocomplete="off"
        spellcheck="false"
      >
        <div class="grid grid-cols-2 gap-6">
          <div class="flex flex-col gap-2">
            <div class="text-base text-gray-800 font-medium">
              Labels
            </div>
            <.inputs_for :let={label} field={f[:labels]}>
              <input type="hidden" name="specs[labels_sort][]" value={label.index} />
              <div class="flex items-start gap-1">
                <.text_field field={label[:key]} placeholder="Name" />
                <.text_field field={label[:value]} placeholder="Value" />

                <span class="tooltip left" data-tooltip="Delete this label">
                  <.icon_button
                    class="text-red-600 font-medium text-sm whitespace-nowrap"
                    type="button"
                    name="specs[labels_drop][]"
                    value={label.index}
                    phx-click={JS.dispatch("change")}
                  >
                    <.remix_icon icon="delete-bin-6-line" />
                  </.icon_button>
                </span>
              </div>
            </.inputs_for>
            <input type="hidden" name="specs[labels_drop][]" />

            <.icon_button
              type="button"
              name="specs[labels_sort][]"
              value="new"
              phx-click={JS.dispatch("change")}
            >
              <.remix_icon icon="add-line" />
            </.icon_button>
          </div>

          <div class="flex flex-col gap-2">
            <div class="text-base text-gray-800 font-medium">
              Annotations
            </div>
            <.inputs_for :let={annotation} field={f[:annotations]}>
              <input type="hidden" name="specs[annotations_sort][]" value={annotation.index} />
              <div class="flex items-start gap-1">
                <.text_field field={annotation[:key]} placeholder="Name" />
                <.text_field field={annotation[:value]} placeholder="Value" />

                <span class="tooltip left" data-tooltip="Delete this annotation">
                  <.icon_button
                    class="text-red-600 font-medium text-sm whitespace-nowrap"
                    type="button"
                    name="specs[annotations_drop][]"
                    value={annotation.index}
                    phx-click={JS.dispatch("change")}
                  >
                    <.remix_icon icon="delete-bin-6-line" />
                  </.icon_button>
                </span>
              </div>
            </.inputs_for>
            <input type="hidden" name="specs[annotations_drop][]" />

            <.icon_button
              type="button"
              name="specs[annotations_sort][]"
              value="new"
              phx-click={JS.dispatch("change")}
            >
              <.remix_icon icon="add-line" />
            </.icon_button>
          </div>
        </div>

        <.text_field field={f[:service_account_name]} label="Service Account Name" />
      </.form>
    </div>
    """
  end

  defp storage_config(assigns) do
    ~H"""
    <div>
      <div class="mt-4 text-base text-gray-800 font-medium">
        Storage
      </div>
      <div class="mt-1 text-gray-700">
        Every time you connect to the runtime, a fresh machine is created.
        In order to persist data and caches, you can optionally mount a
        volume at <code>/home/livebook</code>.
      </div>
      <div class="mt-4 flex flex-col gap-4">
        <div class="flex items-start gap-1">
          <form phx-change="set_home_pvc" phx-target={@myself} class="grow">
            <.select_field
              :if={@rbac_permissions.list_pvc}
              value={@home_pvc}
              name="home_pvc"
              label="Persistent Volume Claim"
              options={[{"None", nil} | @pvcs]}
            />
          </form>

          <div class="mt-7 flex items-center gap-1">
            <span class="tooltip left" data-tooltip="Delete selected PVC">
              <.icon_button
                :if={@rbac_permissions.delete_pvc}
                phx-click="delete_pvc"
                phx-target={@myself}
                disabled={@home_pvc == nil or @pvc_action != nil}
              >
                <.remix_icon icon="delete-bin-6-line" />
              </.icon_button>
            </span>
            <span class="tooltip left" data-tooltip="Create new PVC">
              <.icon_button phx-click="new_pvc" phx-target={@myself}>
                <.remix_icon icon="add-line" />
              </.icon_button>
            </span>
          </div>
        </div>
        <div
          :if={@pvc_action[:type] == :delete}
          class="px-4 py-3 flex space-x-4 items-center border border-gray-200 rounded-lg"
        >
          <p class="grow text-gray-700 text-sm">
            Are you sure you want to irreversibly delete Persistent Volume Claim <span class="font-semibold"><%= @home_pvc %></span>?
          </p>
          <div class="flex space-x-4">
            <button
              class="text-red-600 font-medium text-sm whitespace-nowrap"
              phx-click="confirm_delete_pvc"
              phx-target={@myself}
            >
              <.remix_icon icon="delete-bin-6-line" class="align-middle mr-1" /> Delete
            </button>
            <button
              class="text-gray-600 font-medium text-sm"
              phx-click="cancel_delete_pvc"
              phx-target={@myself}
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
          class="flex gap-2 items-center"
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
          <.button type="submit" disabled={not @pvc_action.changeset.valid?}>
            Create
          </.button>
          <.button type="button" color="gray" outlined phx-click="cancel_new_pvc" phx-target={@myself}>
            Cancel
          </.button>
        </.form>
      </div>
    </div>
    """
  end

  defp save_config_form(assigns) do
    ~H"""
    <.form
      :let={f}
      for={@save_config.changeset}
      as={:secret}
      class="mt-4 flex flex-col"
      phx-change="validate_save_config"
      phx-submit="save_config"
      phx-target={@myself}
      autocomplete="off"
      spellcheck="false"
    >
      <div class="text-lg text-gray-800 font-semibold">
        Save config
      </div>
      <div class="mt-1 text-gray-700">
        Store the config in a secret in the <.workspace hub={@hub} /> workspace to reuse it later.
      </div>
      <div :if={error = @save_config.error} class="mt-4">
        <.message_box kind={:error} message={error} />
      </div>
      <div class="mt-4 grid grid-cols-3">
        <.text_field field={f[:name]} label="Secret name" class="uppercase" autofocus />
      </div>
      <div class="mt-6 flex gap-2">
        <.button type="submit" disabled={not @save_config.changeset.valid? or @save_config.inflight}>
          <%= if(@save_config.inflight, do: "Saving...", else: "Save") %>
        </.button>
        <.button
          color="gray"
          outlined
          type="button"
          phx-click="cancel_save_config"
          phx-target={@myself}
        >
          Cancel
        </.button>
      </div>
    </.form>
    """
  end

  defp workspace(assigns) do
    ~H"""
    <span class="font-medium">
      <span class="text-lg"><%= @hub.hub_emoji %></span>
      <span><%= @hub.hub_name %></span>
    </span>
    """
  end

  defp config_actions(assigns) do
    ~H"""
    <div class="mt-1 flex justify-end gap-1">
      <.button
        color="gray"
        outlined
        small
        type="button"
        phx-click="open_save_config"
        phx-target={@myself}
      >
        Save config
      </.button>
      <.menu id="config-secret-menu">
        <:toggle>
          <.button color="gray" outlined small type="button">
            <span>Load config</span>
            <.remix_icon icon="arrow-down-s-line" class="text-base leading-none" />
          </.button>
        </:toggle>
        <div
          :if={config_secret_names(@hub_secrets) == []}
          class="px-3 py-1 whitespace-nowrap text-gray-600 text-sm"
        >
          No configs saved yet
        </div>
        <.menu_item :for={name <- config_secret_names(@hub_secrets)}>
          <button
            class="text-gray-500 text-sm"
            type="button"
            role="menuitem"
            phx-click={JS.push("load_config", value: %{name: name}, target: @myself)}
          >
            <%= name %>
          </button>
        </.menu_item>
      </.menu>
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
    <.message_box kind={:error}>
      <div class="flex items-center justify-between">
        <div>Authentication with cluster failed.</div>
      </div>
    </.message_box>
    """
  end

  defp cluster_check_error(%{error: %{reason: :timeout}} = assigns) do
    ~H"""
    <.message_box kind={:error}>
      <div class="flex items-center justify-between">
        <div>Connection to cluster timed out.</div>
      </div>
    </.message_box>
    """
  end

  defp cluster_check_error(%{error: %{reason: :timeout}} = assigns) do
    ~H"""
    <.message_box kind={:error}>
      <div class="flex items-center justify-between">
        <div>Connection to cluster failed.</div>
      </div>
    </.message_box>
    """
  end

  defp rbac_error(%{error: %Req.Response{status: 201} = resp} = assigns) do
    resourceAttributes = resp.body["spec"]["resourceAttributes"]
    verb = resourceAttributes["verb"]
    namespace = resourceAttributes["namespace"]

    gkv =
      String.trim(
        "#{resourceAttributes["group"]}/#{resourceAttributes["version"]}/#{resourceAttributes["resource"]}",
        "/"
      )

    assigns = assign(assigns, verb: verb, gkv: gkv, namespace: namespace)

    ~H"""
    <.message_box kind={:error}>
      <div class="flex items-center justify-between">
        <div>
          User has no permission to <span class="font-semibold"><%= @verb %></span>
          <code><%= @gkv %></code>
          <span :if={@namespace}> in namespace <code><%= @namespace %></code> (or the namespace doesn't exist)</span>.
        </div>
      </div>
    </.message_box>
    """
  end

  @impl true
  def handle_event("validate_kubeconfig", _params, socket) do
    {:noreply, socket}
  end

  def handle_event("save_kubeconfig", _params, socket) do
    {:noreply, socket}
  end

  def handle_event("set_context", %{"context" => context}, socket) do
    {:noreply, socket |> set_context(context) |> set_namespace(nil)}
  end

  def handle_event("set_namespace", %{"namespace" => namespace}, socket) do
    {:noreply, set_namespace(socket, namespace)}
  end

  def handle_event("set_home_pvc", %{"home_pvc" => home_pvc}, socket) do
    {:noreply, assign(socket, :home_pvc, home_pvc)}
  end

  def handle_event("new_pvc", %{}, socket) do
    pvc_action = %{
      type: :new,
      changeset: PVC.changeset(%PVC{}),
      storage_classes: storage_classes(socket.assigns),
      inflight: false,
      error: false
    }

    {:noreply, assign(socket, pvc_action: pvc_action)}
  end

  def handle_event("validate_basic_specs", %{"specs" => specs}, socket) do
    changeset =
      specs
      |> Pod.BasicSpecs.changeset()
      |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, :basic_specs_changeset, changeset)}
  end

  def handle_event("validate_advanced_specs", %{"specs" => specs}, socket) do
    changeset =
      specs
      |> Pod.AdvancedSpecs.changeset()
      |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, :advanced_specs_changeset, changeset)}
  end

  def handle_event("validate_pvc", %{"pvc" => pvc}, socket) do
    changeset =
      %PVC{}
      |> PVC.changeset(pvc)
      |> Map.replace!(:action, :validate)

    {:noreply, assign_nested(socket, :pvc_action, changeset: changeset)}
  end

  def handle_event("cancel_new_pvc", %{}, socket) do
    {:noreply, assign(socket, pvc_action: nil)}
  end

  def handle_event("create_pvc", %{"pvc" => pvc}, socket) do
    %PVC{}
    |> PVC.changeset(pvc)
    |> apply_action(:insert)
    |> case do
      {:ok, applied_pvc} ->
        {:noreply, create_pvc(socket, applied_pvc)}

      {:error, changeset} ->
        {:noreply, assign_nested(socket, :pvc_action, changeset: changeset)}
    end
  end

  def handle_event("delete_pvc", %{}, socket) do
    pvc_action = %{type: :delete, error: nil}
    {:noreply, assign(socket, pvc_action: pvc_action)}
  end

  def handle_event("confirm_delete_pvc", %{}, socket) do
    %{namespace: namespace, home_pvc: name} = socket.assigns
    req = socket.assigns.reqs.pvc

    socket =
      case Kubereq.delete(req, namespace, name) do
        {:ok, %{status: 200}} ->
          socket
          |> assign(home_pvc: nil, pvc_action: nil)
          |> pvc_options()

        _ ->
          socket
      end

    {:noreply, socket}
  end

  def handle_event("init", %{}, socket) do
    config = build_config(socket)
    runtime = Runtime.K8s.new(config, socket.assigns.reqs.pod)
    Session.set_runtime(socket.assigns.session.pid, runtime)
    Session.connect_runtime(socket.assigns.session.pid)
    {:noreply, socket}
  end

  def handle_event("open_save_config", %{}, socket) do
    changeset = config_secret_changeset(socket, %{name: @config_secret_prefix})
    save_config = %{changeset: changeset, inflight: false, error: false}
    {:noreply, assign(socket, save_config: save_config)}
  end

  def handle_event("cancel_save_config", %{}, socket) do
    {:noreply, assign(socket, save_config: nil)}
  end

  def handle_event("validate_save_config", %{"secret" => secret}, socket) do
    changeset =
      socket
      |> config_secret_changeset(secret)
      |> Map.replace!(:action, :validate)

    {:noreply, assign_nested(socket, :save_config, changeset: changeset)}
  end

  def handle_event("save_config", %{"secret" => secret}, socket) do
    changeset = config_secret_changeset(socket, secret)

    case Ecto.Changeset.apply_action(changeset, :insert) do
      {:ok, secret} ->
        {:noreply, save_config_secret(socket, secret, changeset)}

      {:error, changeset} ->
        {:noreply, assign_nested(socket, :save_config, changeset: changeset)}
    end
  end

  def handle_event("load_config", %{"name" => name}, socket) do
    secret = Enum.find(socket.assigns.hub_secrets, &(&1.name == name))

    case Jason.decode(secret.value) do
      {:ok, config_defaults} ->
        {:noreply,
         socket
         |> assign(config_defaults: config_defaults)
         |> load_config_defaults()}

      {:error, _} ->
        {:noreply, socket}
    end
  end

  @impl true
  def handle_async(:load_namespace_options, {:ok, result}, socket) do
    socket =
      with :ok <- result,
           {:ok, %Req.Response{status: 200, body: %{"items" => resources}}} <-
             Kubereq.list(socket.assigns.reqs.namespaces, nil) do
        namespace_options = Enum.map(resources, & &1["metadata"]["name"])

        socket
        |> assign(:namespace_options, namespace_options)
        |> set_namespace(List.first(namespace_options))
        |> assign(:cluster_check, %{status: :ok, error: nil})
      else
        {:ok, _response} ->
          # cannot list namespaces
          socket
          |> assign(:namespace_options, nil)
          |> assign(:cluster_check, %{status: :ok, error: nil})

        {:error, error} ->
          # cannot connect to cluster
          socket
          |> assign(:namespace_options, nil)
          |> assign(:cluster_check, %{status: :error, error: error})
      end

    {:noreply, socket}
  end

  def handle_async(:save_config, {:ok, result}, socket) do
    socket =
      case result do
        :ok ->
          assign(socket, save_config: nil)

        {:error, %Ecto.Changeset{} = changeset} ->
          assign_nested(socket, :save_config, changeset: changeset, inflight: false)

        {:transport_error, error} ->
          assign_nested(socket, :save_config, error: error, inflight: false)
      end

    {:noreply, socket}
  end

  # defp label(app_name, runtime, runtime_status) do
  #   reconnecting? = reconnecting?(app_name, runtime)

  #   case {reconnecting?, runtime_status} do
  #     {true, :connected} -> "Reconnect"
  #     {true, :connecting} -> "Connecting..."
  #     _ -> "Connect"
  #   end
  # end

  # defp reconnecting?(app_name, runtime) do
  #   match?(%Runtime.Fly{config: %{app_name: ^app_name}}, runtime)
  # end

  defp create_pvc(socket, pvc) do
    namespace = socket.assigns.namespace
    manifest = PVC.manifest(pvc, namespace)
    req = socket.assigns.reqs.pvc

    case Kubereq.create(req, manifest) do
      {:ok, %{body: created_pvc}} ->
        socket
        |> assign(home_pvc: created_pvc["metadata"]["name"], pvc_action: nil)
        |> pvc_options()
    end
  end

  defp set_context(socket, nil), do: socket

  defp set_context(socket, context) do
    kubeconfig = Kubereq.Kubeconfig.set_current_context(socket.assigns.kubeconfig, context)

    reqs = %{
      access_reviews:
        Kubereq.new(kubeconfig, "apis/authorization.k8s.io/v1/selfsubjectaccessreviews"),
      namespaces: Kubereq.new(kubeconfig, "api/v1/namespaces/:name"),
      pod: Kubereq.new(kubeconfig, "api/v1/namespaces/:namespace/pods/:name"),
      pvc: Kubereq.new(kubeconfig, "api/v1/namespaces/:namespace/persistentvolumeclaims/:name"),
      sc: Kubereq.new(kubeconfig, "apis/storage.k8s.io/v1/storageclasses/:name")
    }

    socket
    |> start_async(:load_namespace_options, fn ->
      Livebook.K8s.Auth.create_access_reviews(reqs)
    end)
    |> assign(
      kubeconfig: kubeconfig,
      context: context,
      namespace: nil,
      namespace_options: nil,
      rbac_error: nil,
      reqs: reqs,
      cluster_check: %{status: :inflight, error: nil}
    )
  end

  defp set_namespace(socket, nil) do
    assign(socket, namespace: nil, rbac_permissions: nil)
  end

  defp set_namespace(socket, ns) do
    reqs = socket.assigns.reqs

    with :ok <- Auth.can_i?(reqs, verb: "create", version: "v1", resource: "pods", namespace: ns),
         :ok <- Auth.can_i?(reqs, verb: "get", version: "v1", resource: "pods", namespace: ns),
         :ok <- Auth.can_i?(reqs, verb: "list", version: "v1", resource: "pods", namespace: ns) do
      rbac_checks = [
        {:get_pvc, {"get", "v1", "persistentvolumeclaims"}},
        {:list_pvc, {"list", "v1", "persistentvolumeclaims"}},
        {:create_pvc, {"create", "v1", "persistentvolumeclaims"}},
        {:delete_pvc, {"delete", "v1", "persistentvolumeclaims"}},
        {:list_sc, {"list", "v1", "storageclasses"}}
      ]

      rbac_permissions =
        for {key, {verb, version, resource}} <- rbac_checks, into: %{} do
          {key,
           :ok ==
             Auth.can_i?(reqs, verb: verb, version: version, resource: resource, namespace: ns)}
        end

      socket
      |> assign(namespace: ns, rbac_permissions: rbac_permissions)
      |> pvc_options()
    else
      {:error, error} -> assign(socket, rbac_error: error)
    end
  end

  defp pvc_options(%{assigns: %{rbac_permissions: %{list_pvc: false}}} = socket) do
    assign(socket, :pvcs, [])
  end

  defp pvc_options(socket) do
    %{reqs: %{pvc: req}, namespace: ns} = socket.assigns

    case Kubereq.list(req, ns) do
      {:ok, %Req.Response{status: 200} = resp} ->
        pvcs =
          resp.body["items"]
          |> Enum.reject(& &1["metadata"]["deletionTimestamp"])
          |> Enum.map(& &1["metadata"]["name"])

        socket
        |> assign(:pvcs, pvcs)

      _ ->
        assign(socket, :pvcs, [])
    end
  end

  defp storage_classes(%{rbac_permissions: %{list_sc: false}}), do: []

  defp storage_classes(assigns) do
    %{reqs: %{sc: req}} = assigns

    case Kubereq.list(req, nil) do
      {:ok, %Req.Response{status: 200} = resp} ->
        Enum.map(resp.body["items"], & &1["metadata"]["name"])

      _ ->
        []
    end
  end

  defp config_secret_names(hub_secrets) do
    names =
      for %{name: name} <- hub_secrets,
          String.starts_with?(name, @config_secret_prefix),
          do: name

    Enum.sort(names)
  end

  defp load_config_defaults(socket) do
    config_defaults = socket.assigns.config_defaults

    socket
    |> assign(
      context: config_defaults["context"],
      namespace: config_defaults["namespace"],
      home_pvc: config_defaults["home_pvc"],
      basic_specs_changeset: Pod.BasicSpecs.changeset(config_defaults["basic_specs"]),
      advanced_specs_changeset: Pod.AdvancedSpecs.changeset(config_defaults["advanced_specs"])
    )
  end

  defp config_secret_changeset(socket, attrs) do
    hub = socket.assigns.hub
    value = socket |> build_config() |> Jason.encode!()
    secret = %Livebook.Secrets.Secret{hub_id: hub.id, name: nil, value: value}

    secret
    |> Livebook.Secrets.change_secret(attrs)
    |> validate_format(:name, ~r/^#{@config_secret_prefix}\w+$/,
      message: "must be in the format #{@config_secret_prefix}*"
    )
  end

  defp save_config_secret(socket, secret, changeset) do
    hub = socket.assigns.hub
    exists? = Enum.any?(socket.assigns.hub_secrets, &(&1.name == secret.name))

    socket
    |> start_async(:save_config, fn ->
      result =
        if exists? do
          Livebook.Hubs.update_secret(hub, secret)
        else
          Livebook.Hubs.create_secret(hub, secret)
        end

      with {:error, errors} <- result do
        {:error,
         changeset
         |> Livebook.Utils.put_changeset_errors(errors)
         |> Map.replace!(:action, :validate)}
      end
    end)
    |> assign_nested(:save_config, inflight: true)
  end

  defp assign_nested(socket, key, keyword) do
    update(socket, key, fn map ->
      Enum.reduce(keyword, map, fn {key, value}, map -> Map.replace!(map, key, value) end)
    end)
  end

  defp build_config(socket) do
    basic_specs = apply_changes(socket.assigns.basic_specs_changeset)
    advanced_specs = apply_changes(socket.assigns.advanced_specs_changeset)

    %{
      context: socket.assigns.context,
      namespace: socket.assigns.namespace,
      home_pvc: socket.assigns.home_pvc,
      basic_specs: %{
        docker_tag: basic_specs.docker_tag,
        resource_limits: Map.from_struct(basic_specs.resource_limits),
        resource_requests: Map.from_struct(basic_specs.resource_requests)
      },
      advanced_specs: %{
        annotations: Enum.map(advanced_specs.annotations, &Map.from_struct(&1)),
        labels: Enum.map(advanced_specs.labels, &Map.from_struct(&1)),
        service_account_name: advanced_specs.service_account_name
      }
    }
  end
end

defmodule LivebookWeb.SessionLive.FlyRuntimeComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  @impl true
  def mount(socket) do
    unless Livebook.Config.runtime_enabled?(Livebook.Runtime.Fly) do
      raise "runtime module not allowed"
    end

    {:ok,
     assign(socket,
       token: nil,
       token_check: %{status: :initial, error: nil},
       org: nil,
       regions: nil,
       app_name: nil,
       app_check: %{status: :initial, error: nil},
       volumes: nil,
       region: nil,
       specs_changeset: specs_changeset(),
       volume_id: nil,
       volume_action: nil,
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

  def update(assigns, socket) do
    socket = assign(socket, assigns)

    socket =
      cond do
        is_map_key(socket.assigns, :config_defaults) ->
          socket

        is_struct(assigns.runtime, Livebook.Runtime.Fly) ->
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
        Start a temporary Fly.io machine with an Elixir node to evaluate code.
        The machine is automatically destroyed, once you disconnect the runtime.
      </p>

      <.live_component
        module={LivebookWeb.SessionLive.SaveRuntimeConfigComponent}
        id="save-runtime-config"
        hub={@hub}
        hub_secrets={@hub_secrets}
        target={{__MODULE__, @id}}
        save_config_payload={@save_config_payload}
        secret_prefix="FLY_RUNTIME_"
      />

      <div :if={@save_config_payload == nil}>
        <form
          class="mt-1 flex flex-col gap-4"
          phx-change="set_token"
          phx-nosubmit
          phx-target={@myself}
        >
          <.password_field name="token" value={@token} label="Token" />
          <.message_box :if={@token == nil} kind="info">
            Go to <a
              class="text-blue-600 hover:text-blue-700"
              href="https://fly.io/dashboard"
              phx-no-format
            >Fly dashboard</a>, click "Tokens" in the left sidebar and create a new
            token for your organization of choice. This functionality is restricted
            to organization admins. Alternatively, you can create an app in the
            organization by running <code>fly app create</code>
            and generate a deploy token in
            the app dashboard.
          </.message_box>
          <.loader :if={@token_check.status == :inflight} />
          <.message_box
            :if={error = @token_check.error}
            kind="error"
            message={"Error: " <> error.message}
          />
        </form>

        <.app_config
          :if={@token_check.status == :ok}
          org_name={@org.name}
          regions={@regions}
          app_name={@app_name}
          app_check={@app_check}
          volumes={@volumes}
          region={@region}
          myself={@myself}
        />

        <div :if={@token_check.status == :ok and @app_check.status == :ok}>
          <.specs_config specs_changeset={@specs_changeset} myself={@myself} />

          <.storage_config
            volumes={@volumes}
            volume_id={@volume_id}
            region={@region}
            volume_action={@volume_action}
            myself={@myself}
          />

          <div class="mt-8">
            <div class="flex gap-2">
              <.button
                phx-click="init"
                phx-target={@myself}
                disabled={
                  @runtime_status == :connecting or not @specs_changeset.valid? or
                    volume_errors(@volume_id, @volumes, @region) != []
                }
              >
                {label(@app_name, @runtime, @runtime_status)}
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
              :if={reconnecting?(@app_name, @runtime) && @runtime_connect_info}
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

  defp app_config(assigns) do
    ~H"""
    <div class="mt-4 flex flex-col gap-4">
      <div class="grid grid-cols-3 gap-2">
        <.text_field name="org" label="Organization" value={@org_name} readonly />
        <form
          phx-change="set_app_name"
          phx-nosubmit
          phx-target={@myself}
          autocomplete="off"
          spellcheck="false"
        >
          <.text_field name="app_name" label="App" value={@app_name} phx-debounce="500" />
        </form>
        <form phx-change="set_region" phx-nosubmit phx-target={@myself}>
          <.select_field
            name="region"
            label="Region"
            value={@region}
            options={region_options(@regions)}
          />
        </form>
      </div>
      <.message_box
        :if={@app_name == nil}
        kind="info"
        message="Specify the app where machines should be created."
      />
      <.loader :if={@app_check.status == :inflight} />
      <.app_check_error
        :if={@app_check.error}
        error={@app_check.error}
        app_name={@app_name}
        myself={@myself}
      />
    </div>
    """
  end

  defp app_check_error(%{error: %{status: 404}} = assigns) do
    ~H"""
    <.message_box kind="info">
      <div class="flex items-center justify-between">
        <div>
          App <span class="font-semibold">{@app_name}</span> does not exist yet.
        </div>
        <.button phx-click="create_app" phx-target={@myself}>
          Create
        </.button>
      </div>
    </.message_box>
    """
  end

  defp app_check_error(%{error: %{status: 403}} = assigns) do
    ~H"""
    <.message_box
      kind="error"
      message={
        "This app name is already taken, pick a different name." <>
          " If this is an app you own, enter a token for the corresponding organization."
      }
    />
    """
  end

  defp app_check_error(assigns) do
    ~H"""
    <.message_box kind="error" message={"Error: " <> @error.message} />
    """
  end

  defp specs_config(assigns) do
    ~H"""
    <div class="mt-8">
      <div class="text-lg text-gray-800 font-semibold">
        Specs
      </div>
      <div class="mt-1 text-gray-700">
        For more details refer to
        <a
          class="text-blue-600 hover:text-blue-700"
          href="https://fly.io/docs/machines/guides-examples/machine-sizing"
        >
          Machine sizing
        </a>
        and
        <a class="text-blue-600 hover:text-blue-700" href="https://fly.io/docs/about/pricing">
          Pricing
        </a>
        pages in the Fly.io documentation.
      </div>
      <.form
        :let={f}
        for={@specs_changeset}
        as={:specs}
        class="mt-4 flex flex-col gap-4"
        phx-change="validate_specs"
        phx-nosubmit
        phx-target={@myself}
        autocomplete="off"
        spellcheck="false"
      >
        <div class="grid grid-cols-5 gap-2">
          <.select_field field={f[:cpu_kind]} label="CPU kind" options={cpu_kind_options()} />
          <.text_field field={f[:cpus]} label="CPUs" type="number" min="1" />
          <.text_field field={f[:memory_gb]} label="Memory (GB)" type="number" step="1" min="1" />
          <.select_field field={f[:gpu_kind]} label="GPU kind" options={gpu_kind_options()} />
          <.text_field
            field={f[:gpus]}
            label="GPUs"
            type="number"
            min="1"
            disabled={get_field(@specs_changeset, :gpu_kind) == nil}
          />
          <div class="col-span-5 text-sm text-gray-700">
            GPUs are available only in certain regions, see
            <a
              class="text-blue-600 hover:text-blue-700"
              href="https://fly.io/docs/gpus/getting-started-gpus/#specify-the-region"
            >
              Getting started with GPUs.
            </a>
          </div>
        </div>
        <.radio_field
          field={f[:docker_tag]}
          label="Base Docker image"
          options={LivebookWeb.AppComponents.docker_tag_options()}
        />
      </.form>
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
        volume at <code>/home/livebook</code>.
        Keep in mind that volumes are billed even when not in use, so you
        may want to remove those no longer needed.
      </div>
      <div class="mt-4 flex flex-col gap-4">
        <div class="flex items-start gap-1">
          <div class="grow">
            <form phx-change="set_volume_id" phx-nosubmit phx-target={@myself}>
              <.select_field
                name="volume_id"
                label="Volume"
                value={@volume_id}
                options={[{"None", ""} | volume_options(@volumes)]}
                errors={volume_errors(@volume_id, @volumes, @region)}
              />
            </form>
          </div>
          <div class="mt-7 flex items-center gap-1">
            <span class="tooltip left" data-tooltip="Delete selected volume">
              <.icon_button
                phx-click="delete_volume"
                phx-target={@myself}
                disabled={
                  @volume_id == nil or (@volume_action != nil and @volume_action.status == :inflight)
                }
              >
                <.remix_icon icon="delete-bin-6-line" />
              </.icon_button>
            </span>
            <span class="tooltip left" data-tooltip="Create new volume">
              <.icon_button phx-click="new_volume" phx-target={@myself}>
                <.remix_icon icon="add-line" />
              </.icon_button>
            </span>
          </div>
        </div>
        <div
          :if={@volume_action[:type] == :delete}
          class="px-4 py-3 flex space-x-4 items-center border border-gray-200 rounded-lg"
        >
          <p class="grow text-gray-700 text-sm">
            Are you sure you want to irreversibly delete <span class="font-semibold">{@volume_id}</span>?
          </p>
          <div class="flex space-x-4">
            <button
              class="text-red-600 font-medium text-sm whitespace-nowrap"
              phx-click="confirm_delete_volume"
              phx-target={@myself}
              disabled={@volume_action.status == :inflight}
            >
              <.remix_icon icon="delete-bin-6-line" class="align-middle mr-1" /> Delete
            </button>
            <button
              class="text-gray-600 font-medium text-sm"
              phx-click="cancel_delete_volume"
              phx-target={@myself}
              disabled={@volume_action.status == :inflight}
            >
              Cancel
            </button>
          </div>
        </div>
        <.form
          :let={f}
          :if={@volume_action[:type] == :new}
          for={@volume_action.changeset}
          as={:volume}
          phx-submit="create_volume"
          phx-change="validate_volume"
          phx-target={@myself}
          class="flex gap-2 items-center"
          autocomplete="off"
          spellcheck="false"
        >
          <div>
            <.remix_icon icon="corner-down-right-line" class="text-gray-400 text-lg" />
          </div>
          <div class="grid grid-cols-2 gap-2 grow">
            <.text_field field={f[:name]} placeholder="Name" />
            <.text_field field={f[:size_gb]} placeholder="Size (GB)" type="number" min="1" />
          </div>
          <.button
            type="submit"
            disabled={not @volume_action.changeset.valid? or @volume_action.status == :inflight}
          >
            {if(@volume_action.status == :inflight, do: "Creating...", else: "Create")}
          </.button>
          <.button
            type="button"
            color="gray"
            outlined
            phx-click="cancel_new_volume"
            phx-target={@myself}
            disabled={@volume_action.status == :inflight}
          >
            Cancel
          </.button>
        </.form>
        <div :if={@volume_action[:status] == :error}>
          <.message_box kind="error" message={"Error: " <> @volume_action.error.message} />
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("set_token", %{"token" => token}, socket) do
    {:noreply, socket |> assign(token: nullify(token)) |> load_org_and_regions()}
  end

  def handle_event("set_app_name", %{"app_name" => app_name}, socket) do
    {:noreply, socket |> assign(app_name: nullify(app_name)) |> load_app()}
  end

  def handle_event("set_region", %{"region" => region}, socket) do
    {:noreply, assign(socket, region: region)}
  end

  def handle_event("create_app", %{}, socket) do
    {:noreply, create_app(socket)}
  end

  def handle_event("set_volume_id", %{"volume_id" => volume_id}, socket) do
    {:noreply, assign(socket, volume_id: nullify(volume_id), volume_action: nil)}
  end

  def handle_event("delete_volume", %{}, socket) do
    volume_action = %{type: :delete, status: :initial, error: nil}
    {:noreply, assign(socket, volume_action: volume_action)}
  end

  def handle_event("cancel_delete_volume", %{}, socket) do
    {:noreply, assign(socket, volume_action: nil)}
  end

  def handle_event("confirm_delete_volume", %{}, socket) do
    {:noreply, delete_volume(socket)}
  end

  def handle_event("new_volume", %{}, socket) do
    volume_action = %{type: :new, changeset: volume_changeset(), status: :initial, error: nil}
    {:noreply, assign(socket, volume_action: volume_action)}
  end

  def handle_event("cancel_new_volume", %{}, socket) do
    {:noreply, assign(socket, volume_action: nil)}
  end

  def handle_event("validate_volume", %{"volume" => volume}, socket) do
    changeset =
      volume
      |> volume_changeset()
      |> Map.replace!(:action, :validate)

    {:noreply, assign_nested(socket, :volume_action, changeset: changeset)}
  end

  def handle_event("create_volume", %{"volume" => volume}, socket) do
    volume
    |> volume_changeset()
    |> apply_action(:insert)
    |> case do
      {:ok, %{name: name, size_gb: size_gb}} ->
        {:noreply, create_volume(socket, name, size_gb)}

      {:error, changeset} ->
        {:noreply, assign_nested(socket, :volume_action, changeset: changeset)}
    end
  end

  def handle_event("validate_specs", %{"specs" => specs}, socket) do
    changeset =
      specs
      |> specs_changeset()
      |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, specs_changeset: changeset)}
  end

  def handle_event("init", %{}, socket) do
    config = build_config(socket)
    runtime = Livebook.Runtime.Fly.new(config)
    Livebook.Session.set_runtime(socket.assigns.session.pid, runtime)
    Livebook.Session.connect_runtime(socket.assigns.session.pid)
    {:noreply, socket}
  end

  def handle_event("disconnect", %{}, socket) do
    Livebook.Session.disconnect_runtime(socket.assigns.session.pid)
    {:noreply, socket}
  end

  @impl true
  def handle_async(:load_org_and_regions, {:ok, result}, socket) do
    socket =
      case result do
        {:ok, %{orgs: [org]} = data} ->
          region = socket.assigns.config_defaults["region"] || data.closest_region

          socket
          |> assign(org: org, regions: data.regions, region: region)
          |> assign(:token_check, %{status: :ok, error: nil})

        {:ok, %{orgs: orgs}} ->
          error =
            "expected organization-specific auth token, but the given one gives access to #{length(orgs)} organizations"

          assign(socket, :token_check, %{status: :error, error: error})

        {:error, error} ->
          assign(socket, :token_check, %{status: :error, error: error})
      end

    {:noreply, socket}
  end

  def handle_async(:load_app, {:ok, result}, socket) do
    socket =
      case result do
        {:ok, volumes} ->
          volume_id =
            if volume_id = socket.assigns.config_defaults["volume_id"] do
              # Ignore the volume if it no longer exists
              if Enum.any?(volumes, &(&1.id == volume_id)), do: volume_id
            end

          socket
          |> assign(volumes: volumes, volume_id: volume_id)
          |> assign(:app_check, %{status: :ok, error: nil})

        {:error, error} ->
          assign(socket, :app_check, %{status: :error, error: error})
      end

    {:noreply, socket}
  end

  def handle_async(:create_app, {:ok, result}, socket) do
    socket =
      case result do
        :ok ->
          socket
          |> assign(volumes: [], volume_id: nil)
          |> assign(:app_check, %{status: :ok, error: nil})

        {:error, error} ->
          assign(socket, :app_check, %{status: :error, error: error})
      end

    {:noreply, socket}
  end

  def handle_async(:create_volume, {:ok, result}, socket) do
    socket =
      case result do
        {:ok, volume} ->
          volumes = [volume | socket.assigns.volumes]
          assign(socket, volumes: volumes, volume_id: volume.id, volume_action: nil)

        {:error, error} ->
          assign_nested(socket, :volume_action, error: error, status: :error)
      end

    {:noreply, socket}
  end

  def handle_async(:delete_volume, {:ok, result}, socket) do
    volume_id = socket.assigns.volume_id

    socket =
      case result do
        :ok ->
          volumes = Enum.reject(socket.assigns.volumes, &(&1.id == volume_id))
          assign(socket, volumes: volumes, volume_id: nil, volume_action: nil)

        {:error, error} ->
          assign_nested(socket, :volume_action, error: error, status: :error)
      end

    {:noreply, socket}
  end

  defp label(app_name, runtime, runtime_status) do
    reconnecting? = reconnecting?(app_name, runtime)

    case {reconnecting?, runtime_status} do
      {true, :connected} -> "Reconnect"
      {true, :connecting} -> "Connecting..."
      _ -> "Connect"
    end
  end

  defp reconnecting?(app_name, runtime) do
    match?(%Livebook.Runtime.Fly{config: %{app_name: ^app_name}}, runtime)
  end

  defp cpu_kind_options() do
    Enum.map(Livebook.FlyAPI.cpu_kinds(), &{&1, &1})
  end

  defp gpu_kind_options() do
    [{"None", ""}] ++ Enum.map(Livebook.FlyAPI.gpu_kinds(), &{&1, &1})
  end

  defp region_options(regions) do
    for region <- regions,
        do: {"#{region.name} (#{region.code})", region.code}
  end

  defp volume_options(volumes) do
    for volume <- Enum.sort_by(volumes, &{&1.name, &1.id}),
        do: {
          "#{volume.id} (name: #{volume.name}, region: #{volume.region}, size: #{volume.size_gb} GB)",
          volume.id
        }
  end

  defp load_config_defaults(socket) do
    config_defaults = socket.assigns.config_defaults

    socket
    |> assign(
      token: config_defaults["token"],
      app_name: config_defaults["app_name"],
      specs_changeset: specs_changeset(config_defaults)
    )
    |> load_org_and_regions()
    |> load_app()
  end

  defp specs_changeset(attrs \\ %{}) do
    docker_tags = Enum.map(Livebook.Config.docker_images(), & &1.tag)

    data = %{
      cpu_kind: "shared",
      cpus: 1,
      memory_gb: 1,
      gpu_kind: nil,
      gpus: nil,
      docker_tag: hd(docker_tags)
    }

    types = %{
      cpu_kind: :string,
      cpus: :integer,
      memory_gb: :integer,
      gpu_kind: :string,
      gpus: :integer,
      docker_tag: :string
    }

    changeset =
      cast({data, types}, attrs, Map.keys(types))
      |> validate_required([:cpu_kind, :cpus, :memory_gb, :docker_tag])
      |> validate_inclusion(:docker_tag, docker_tags)

    if get_field(changeset, :gpu_kind) do
      changeset
    else
      # We may be reverting back to the defult, so we force the change
      # to take precedence over form params in Phoenix.HTML.FormData
      force_change(changeset, :gpus, nil)
    end
  end

  defp volume_changeset(attrs \\ %{}) do
    data = %{name: nil, size_gb: nil}

    types = %{
      name: :string,
      size_gb: :integer
    }

    cast({data, types}, attrs, Map.keys(types))
    |> validate_required([:name, :size_gb])
  end

  defp volume_errors(nil, _volumes, _region), do: []

  defp volume_errors(volume_id, volumes, region) do
    volume = Enum.find(volumes, &(&1.id == volume_id))

    if volume.region == region do
      []
    else
      ["must be in the same region as the machine (#{region})"]
    end
  end

  defp load_org_and_regions(socket) when socket.assigns.token == nil do
    assign(socket, :token_check, %{status: :initial, error: nil})
  end

  defp load_org_and_regions(socket) do
    token = socket.assigns.token

    socket
    |> start_async(:load_org_and_regions, fn ->
      Livebook.FlyAPI.get_orgs_and_regions(token)
    end)
    |> assign(:token_check, %{status: :inflight, error: nil})
  end

  defp load_app(socket) when socket.assigns.app_name == nil do
    assign(socket, :app_check, %{status: :initial, error: nil})
  end

  defp load_app(socket) do
    %{token: token, app_name: app_name} = socket.assigns

    socket
    |> start_async(:load_app, fn ->
      Livebook.FlyAPI.get_app_volumes(token, app_name)
    end)
    |> assign(:app_check, %{status: :inflight, error: nil})
  end

  defp create_app(socket) do
    %{token: token, app_name: app_name} = socket.assigns
    org_slug = socket.assigns.org.slug

    socket
    |> start_async(:create_app, fn ->
      Livebook.FlyAPI.create_app(token, app_name, org_slug)
    end)
    |> assign(:app_check, %{status: :inflight, error: nil})
  end

  defp delete_volume(socket) do
    %{token: token, app_name: app_name, volume_id: volume_id} = socket.assigns

    socket
    |> start_async(:delete_volume, fn ->
      Livebook.FlyAPI.delete_volume(token, app_name, volume_id)
    end)
    |> assign_nested(:volume_action, status: :inflight)
  end

  defp create_volume(socket, name, size_gb) do
    %{token: token, app_name: app_name, region: region} = socket.assigns

    specs = apply_changes(socket.assigns.specs_changeset)

    compute = %{
      cpu_kind: specs.cpu_kind,
      cpus: specs.cpus,
      memory_mb: specs.memory_gb * 1024,
      gpu_kind: specs.gpu_kind,
      gpus: specs.gpus
    }

    socket
    |> start_async(:create_volume, fn ->
      Livebook.FlyAPI.create_volume(token, app_name, name, region, size_gb, compute)
    end)
    |> assign_nested(:volume_action, status: :inflight)
  end

  defp build_config(socket) do
    specs = apply_changes(socket.assigns.specs_changeset)

    %{
      token: socket.assigns.token,
      app_name: socket.assigns.app_name,
      region: socket.assigns.region,
      cpu_kind: specs.cpu_kind,
      cpus: specs.cpus,
      memory_gb: specs.memory_gb,
      gpu_kind: specs.gpu_kind,
      gpus: specs.gpus,
      volume_id: socket.assigns.volume_id,
      docker_tag: specs.docker_tag
    }
  end

  defp nullify(""), do: nil
  defp nullify(value), do: value
end

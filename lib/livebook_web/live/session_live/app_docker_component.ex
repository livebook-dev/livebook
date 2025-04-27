defmodule LivebookWeb.SessionLive.AppDockerComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  alias Livebook.Hubs
  alias Livebook.FileSystem
  alias LivebookWeb.AppComponents
  alias Livebook.Hubs.Provider

  @impl true
  def update(assigns, socket) do
    deployment_group_changed? =
      not Map.has_key?(socket.assigns, :deployment_group_id) or
        socket.assigns.deployment_group_id != assigns.deployment_group_id

    socket = assign(socket, assigns)
    deployment_groups = Provider.deployment_groups(assigns.hub)

    deployment_group =
      if assigns.deployment_group_id do
        Enum.find(deployment_groups, &(&1.id == assigns.deployment_group_id))
      end

    socket =
      socket
      |> assign(settings_valid?: Livebook.Notebook.AppSettings.valid?(socket.assigns.settings))
      |> assign(
        hub_secrets: Hubs.get_secrets(assigns.hub),
        hub_file_systems: Hubs.get_file_systems(assigns.hub, hub_only: true),
        deployment_groups: deployment_groups,
        deployment_group: deployment_group,
        deployment_group_id: assigns.deployment_group_id
      )
      |> assign_new(:messages, fn -> [] end)

    socket =
      if deployment_group_changed? do
        assign(socket,
          changeset: Hubs.Dockerfile.config_changeset(base_config(socket)),
          deployment_type: :dockerfile
        )
      else
        socket
      end

    {:ok, update_dockerfile(socket)}
  end

  defp base_config(socket) do
    if deployment_group = socket.assigns.deployment_group do
      Hubs.Dockerfile.from_deployment_group(deployment_group)
    else
      Hubs.Dockerfile.config_new()
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        App deployment with Docker
      </h3>
      <.content
        file={@file}
        settings_valid?={@settings_valid?}
        hub={@hub}
        deployment_group={@deployment_group}
        deployment_groups={@deployment_groups}
        deployment_group_id={@deployment_group_id}
        changeset={@changeset}
        session={@session}
        dockerfile={@dockerfile}
        messages={@messages}
        myself={@myself}
      />
    </div>
    """
  end

  defp content(%{file: nil} = assigns) do
    ~H"""
    <div class="flex justify-between">
      <p class="text-gray-700">
        To deploy this app, make sure to save the notebook first.
      </p>
      <.link
        class="text-blue-600 font-medium"
        patch={~p"/sessions/#{@session.id}/settings/file?context=app-docker"}
      >
        <span>Save</span>
        <.remix_icon icon="arrow-right-line" />
      </.link>
    </div>
    """
  end

  defp content(%{settings_valid?: false} = assigns) do
    ~H"""
    <div class="flex justify-between">
      <p class="text-gray-700">
        To deploy this app, make sure to specify valid settings.
      </p>
      <.link
        class="text-blue-600 font-medium"
        patch={~p"/sessions/#{@session.id}/settings/app?context=app-docker"}
      >
        <span>Configure</span>
        <.remix_icon icon="arrow-right-line" />
      </.link>
    </div>
    """
  end

  defp content(assigns) do
    ~H"""
    <div class="flex flex-col gap-4">
      <p class="text-gray-700 pb-4">
        Choose your deployment settings and then deploy your notebook using the generated Dockerfile.
      </p>

      <div class="flex gap-12">
        <p class="text-gray-700">
          <.label>Workspace</.label>
          <span>
            <span class="text-lg">{@hub.hub_emoji}</span>
            <span>{@hub.hub_name}</span>
          </span>
        </p>
        <%= if @deployment_groups do %>
          <%= if @deployment_groups != [] do %>
            <.form
              :let={f}
              for={%{"id" => @deployment_group_id}}
              as={:deployment_group}
              phx-change="select_deployment_group"
              phx-target={@myself}
              id="select_deployment_group_form"
            >
              <.select_field
                help={deployment_group_help()}
                field={f[:id]}
                options={deployment_group_options(@deployment_groups)}
                label="Deployment Group"
              />
            </.form>
          <% else %>
            <p class="text-gray-700">
              <.label help={deployment_group_help()}>
                Deployment Group
              </.label>
              <span>
                None configured
                <.link
                  navigate={~p"/hub/#{@hub.id}/groups/new"}
                  target="_blank"
                  class="pl-3 text-blue-600 font-semibold"
                >
                  + add new
                </.link>
              </span>
            </p>
          <% end %>
        <% end %>
      </div>

      <div :if={@messages != []} class="flex flex-col gap-2">
        <.message_box :for={{kind, message} <- @messages} kind={kind}>
          {raw(message)}
        </.message_box>
      </div>

      <.form :let={f} for={@changeset} as={:data} phx-change="validate" phx-target={@myself}>
        <div class="flex flex-col space-y-4">
          <AppComponents.deployment_group_form_content
            hub={@hub}
            form={f}
            disabled={@deployment_group_id != nil}
          />
          <div class="flex flex-col space-y-4">
            <.radio_field
              label="Deploy"
              field={f[:deploy_all]}
              options={[
                {"false", "Only this notebook"},
                {"true", "All notebooks in the current directory"}
              ]}
            />
            <.radio_field
              label="Base image"
              field={f[:docker_tag]}
              options={AppComponents.docker_tag_options()}
            />
          </div>
        </div>
      </.form>

      <div class="flex flex-col gap-4 pt-6">
        <div>
          <div class="flex items-end mb-1 gap-1">
            <span class="text-sm text-gray-700 font-semibold">Dockerfile</span>
            <div class="grow" />
            <.button
              color="gray"
              small
              type="button"
              aria-label="save dockerfile alongside the notebook"
              phx-click="save_dockerfile"
              phx-target={@myself}
            >
              <.remix_icon icon="save-line" />
              <span>Save alongside notebook</span>
            </.button>
            <.button
              color="gray"
              small
              data-tooltip="Copied to clipboard"
              type="button"
              aria-label="copy to clipboard"
              phx-click={
                JS.dispatch("lb:clipcopy", to: "#dockerfile-source")
                |> JS.transition("tooltip top", time: 2000)
              }
            >
              <.remix_icon icon="clipboard-line" />
              <span>Copy source</span>
            </.button>
          </div>

          <.code_preview source_id="dockerfile-source" source={@dockerfile} language="dockerfile" />
        </div>

        <div class="text-gray-700">
          To test the deployment locally, go the the notebook directory, save the Dockerfile, then run:
        </div>

        <.code_preview
          source_id="dockerfile-cmd"
          source={
            ~s'''
            docker build -t my-app .
            docker run --rm -p 8080:8080 -p 8081:8081 my-app
            '''
          }
          language="text"
        />

        <p class="text-gray-700 py-2">
          You may additionally perform the following optional steps:
        </p>

        <ul class="text-gray-700 space-y-3">
          <li :if={Hubs.Provider.type(@hub) == "team"} class="flex gap-2">
            <div><.remix_icon icon="arrow-right-line" class="text-gray-900" /></div>
            <span>
              you may remove the default value for <code>TEAMS_KEY</code>
              from your Dockerfile and set it as a build argument in your deployment
              platform
            </span>
          </li>
          <li :if={apply_changes(@changeset).clustering} class="flex gap-2">
            <div><.remix_icon icon="arrow-right-line" class="text-gray-900" /></div>
            <span>
              you may set <code>LIVEBOOK_SECRET_KEY_BASE</code>
              and <code>LIVEBOOK_COOKIE</code>
              as runtime environment secrets in your deployment platform, to ensure their
              values stay the same across deployments. If you do that, you can remove
              the defaults from your Dockerfile
            </span>
          </li>
          <li class="flex gap-2">
            <div><.remix_icon icon="arrow-right-line" class="text-gray-900" /></div>
            <span>
              if you want to debug your deployed notebooks in production, you may
              set the <code>LIVEBOOK_PASSWORD</code> environment variable with a
              value of at least 12 characters of your choice
            </span>
          </li>
        </ul>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"data" => data}, socket) do
    changeset =
      socket
      |> base_config()
      |> Hubs.Dockerfile.config_changeset(data)
      |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, changeset: changeset) |> update_dockerfile()}
  end

  def handle_event("save_dockerfile", %{}, socket) do
    dockerfile_file = FileSystem.File.resolve(socket.assigns.file, "./Dockerfile")
    file_path_message = "File saved at #{dockerfile_file.path}"

    case FileSystem.File.write(dockerfile_file, socket.assigns.dockerfile) do
      :ok -> {:noreply, assign(socket, messages: [{"info", file_path_message}])}
      {:error, message} -> {:noreply, assign(socket, messages: [{"error", message}])}
    end
  end

  def handle_event("select_deployment_group", %{"deployment_group" => %{"id" => id}}, socket) do
    id = if(id != "", do: id)
    Livebook.Session.set_notebook_deployment_group(socket.assigns.session.pid, id)

    {:noreply, socket}
  end

  defp update_dockerfile(socket) when socket.assigns.file == nil do
    assign(socket, dockerfile: nil, messages: [])
  end

  defp update_dockerfile(socket) do
    config = apply_changes(socket.assigns.changeset)

    %{
      hub: hub,
      hub_secrets: hub_secrets,
      hub_file_systems: hub_file_systems,
      file: file,
      file_entries: file_entries,
      secrets: secrets,
      app_settings: app_settings,
      deployment_groups: deployment_groups,
      deployment_group_id: deployment_group_id
    } = socket.assigns

    deployment_group =
      if deployment_group_id, do: Enum.find(deployment_groups, &(&1.id == deployment_group_id))

    hub_secrets =
      if deployment_group do
        Enum.uniq_by(deployment_group.secrets ++ hub_secrets, & &1.name)
      else
        hub_secrets
      end

    dockerfile =
      Hubs.Dockerfile.airgapped_dockerfile(
        config,
        hub,
        hub_secrets,
        hub_file_systems,
        file,
        file_entries,
        secrets
      )

    warnings =
      Hubs.Dockerfile.airgapped_warnings(
        config,
        hub,
        hub_secrets,
        hub_file_systems,
        app_settings,
        file_entries,
        secrets
      )

    messages = Enum.map(warnings, &{"warning", &1})

    assign(socket, dockerfile: dockerfile, messages: messages)
  end

  defp deployment_group_options(deployment_groups) do
    [{"none", nil}] ++
      for deployment_group <- deployment_groups do
        {"#{deployment_group.name} (#{mode(deployment_group.mode)})", deployment_group.id}
      end
  end

  defp mode(:online), do: "online"
  defp mode(:offline), do: "airgapped"

  defp deployment_group_help() do
    "Share deployment credentials, secrets, and configuration with deployment groups."
  end
end

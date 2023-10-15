defmodule LivebookWeb.AppHelpers do
  use LivebookWeb, :html

  import Ecto.Changeset

  alias Livebook.Hubs

  @doc """
  Renders page placeholder on unauthenticated dead render.
  """
  def auth_placeholder(assigns) do
    ~H"""
    <div class="flex justify-center items-center h-screen w-screen">
      <img src={~p"/images/logo.png"} height="128" width="128" alt="livebook" class="animate-pulse" />
    </div>
    """
  end

  @doc """
  Renders app status with indicator.
  """
  attr :status, :map, required: true
  attr :show_label, :boolean, default: true

  def app_status(%{status: %{lifecycle: :shutting_down}} = assigns) do
    ~H"""
    <.app_status_indicator text={@show_label && "Shutting down"} variant={:inactive} />
    """
  end

  def app_status(%{status: %{lifecycle: :deactivated}} = assigns) do
    ~H"""
    <.app_status_indicator text={@show_label && "Deactivated"} variant={:inactive} />
    """
  end

  def app_status(%{status: %{execution: :executing}} = assigns) do
    ~H"""
    <.app_status_indicator text={@show_label && "Executing"} variant={:progressing} />
    """
  end

  def app_status(%{status: %{execution: :executed}} = assigns) do
    ~H"""
    <.app_status_indicator text={@show_label && "Executed"} variant={:success} />
    """
  end

  def app_status(%{status: %{execution: :error}} = assigns) do
    ~H"""
    <.app_status_indicator text={@show_label && "Error"} variant={:error} />
    """
  end

  def app_status(%{status: %{execution: :interrupted}} = assigns) do
    ~H"""
    <.app_status_indicator text={@show_label && "Interrupted"} variant={:waiting} />
    """
  end

  defp app_status_indicator(assigns) do
    ~H"""
    <span class="flex items-center space-x-2">
      <.status_indicator variant={@variant} />
      <span :if={@text}><%= @text %></span>
    </span>
    """
  end

  @doc """
  Shows a confirmation modal and closes the app on confirm.
  """
  def confirm_app_termination(socket, app_pid) do
    on_confirm = fn socket ->
      Livebook.App.close(app_pid)
      socket
    end

    confirm(socket, on_confirm,
      title: "Terminate app",
      description: "All app sessions will be immediately terminated.",
      confirm_text: "Terminate",
      confirm_icon: "delete-bin-6-line"
    )
  end

  @doc """
  Builds a changeset for app Dockerfile configuration.
  """
  @spec docker_config_changeset(map()) :: Ecto.Changeset.t()
  def docker_config_changeset(attrs \\ %{}) do
    default_image = Livebook.Config.docker_images() |> hd()

    data = %{deploy_all: false, docker_tag: default_image.tag, zta_provider: nil, zta_key: nil}

    zta_types =
      for provider <- Livebook.Config.identity_providers(),
          not provider.read_only,
          do: provider.type

    types = %{
      deploy_all: :boolean,
      docker_tag: :string,
      zta_provider: Ecto.ParameterizedType.init(Ecto.Enum, values: zta_types),
      zta_key: :string
    }

    cast({data, types}, attrs, [:deploy_all, :docker_tag, :zta_provider, :zta_key])
    |> validate_required([:deploy_all, :docker_tag])
  end

  @doc """
  Renders form fields for Dockerfile configuration.
  """
  attr :form, Phoenix.HTML.Form, required: true
  attr :hub, :map, required: true
  attr :show_deploy_all, :boolean, default: true

  def docker_config_form_content(assigns) do
    ~H"""
    <div class="flex flex-col space-y-4">
      <.radio_field
        :if={@show_deploy_all}
        label="Deploy"
        field={@form[:deploy_all]}
        options={[
          {"false", "Only this notebook"},
          {"true", "All notebooks in the current directory"}
        ]}
      />
      <.radio_field label="Base image" field={@form[:docker_tag]} options={docker_tag_options()} />
      <%= if Hubs.Provider.type(@hub) == "team" do %>
        <div class="flex flex-col">
          <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
            <.select_field
              label="Zero Trust Authentication provider"
              field={@form[:zta_provider]}
              help="Enable this option if you want to deploy your notebooks behind an authentication proxy"
              prompt="None"
              options={zta_options()}
            />
            <.text_field
              :if={zta_metadata = zta_metadata(@form[:zta_provider].value)}
              field={@form[:zta_key]}
              label={zta_metadata.value}
              phx-debounce
            />
          </div>
          <div :if={zta_metadata = zta_metadata(@form[:zta_provider].value)} class="text-sm mt-1">
            See the
            <a
              class="text-blue-800 hover:text-blue-600"
              href={"https://hexdocs.pm/livebook/#{zta_metadata.type}"}
            >
              Authentication with <%= zta_metadata.name %> docs
            </a>
            for more information.
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  @zta_options for provider <- Livebook.Config.identity_providers(),
                   not provider.read_only,
                   do: {provider.name, provider.type}

  defp zta_options(), do: @zta_options

  defp docker_tag_options() do
    for image <- Livebook.Config.docker_images(), do: {image.tag, image.name}
  end

  @doc """
  Renders Docker deployment instruction for an app.
  """
  attr :hub, :map, required: true
  attr :dockerfile, :string, required: true

  slot :dockerfile_actions, default: nil

  def docker_instructions(assigns) do
    ~H"""
    <div class="flex flex-col gap-4">
      <div>
        <div class="flex items-end mb-1 gap-1">
          <span class="text-sm text-gray-700 font-semibold">Dockerfile</span>
          <div class="grow" />
          <%= render_slot(@dockerfile_actions) %>
          <button
            class="button-base button-gray whitespace-nowrap py-1 px-2"
            data-tooltip="Copied to clipboard"
            type="button"
            aria-label="copy to clipboard"
            phx-click={
              JS.dispatch("lb:clipcopy", to: "#dockerfile-source")
              |> JS.add_class("", transition: {"tooltip top", "", ""}, time: 2000)
            }
          >
            <.remix_icon icon="clipboard-line" class="align-middle mr-1 text-xs" />
            <span class="font-normal text-xs">Copy source</span>
          </button>
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
    """
  end

  defp zta_metadata(nil), do: nil

  defp zta_metadata(zta_provider) do
    Enum.find(Livebook.Config.identity_providers(), &(&1.type == zta_provider))
  end

  @doc """
  Builds Dockerfile definition for app deployment.
  """
  @spec build_dockerfile(
          map(),
          Hubs.Provider.t(),
          list(Livebook.Secrets.Secret.t()),
          list(Livebook.FileSystem.t()),
          Livebook.FileSystem.File.t() | nil,
          list(Livebook.Notebook.file_entry()),
          list(Livebook.Session.Data.secrets())
        ) :: String.t()
  def build_dockerfile(config, hub, hub_secrets, hub_file_systems, file, file_entries, secrets) do
    base_image = Enum.find(Livebook.Config.docker_images(), &(&1.tag == config.docker_tag))

    image = """
    FROM ghcr.io/livebook-dev/livebook:#{base_image.tag}
    """

    image_envs = format_envs(base_image.env)

    hub_config =
      case Hubs.Provider.type(hub) do
        "team" ->
          format_team_hub_config(config, hub, hub_secrets, hub_file_systems)

        "personal" ->
          format_personal_hub_config(config, hub, hub_secrets, secrets)
      end

    apps_config = """
    # Apps configuration
    ENV LIVEBOOK_APPS_PATH "/apps"
    ENV LIVEBOOK_APPS_PATH_WARMUP "manual"
    ENV LIVEBOOK_APPS_PATH_HUB_ID "#{hub.id}"
    """

    notebook =
      if config.deploy_all do
        """
        # Notebooks and files
        COPY . /apps
        """
      else
        notebook_file_name = Livebook.FileSystem.File.name(file)

        notebook =
          """
          # Notebook
          COPY #{notebook_file_name} /apps/
          """

        attachments =
          file_entries
          |> Enum.filter(&(&1.type == :attachment))
          |> Enum.sort_by(& &1.name)

        if attachments == [] do
          notebook
        else
          list = Enum.map_join(attachments, " ", &"files/#{&1.name}")

          """
          # Files
          COPY #{list} /apps/files/

          #{notebook}\
          """
        end
      end

    apps_warmup = """
    # Cache apps setup at build time
    RUN /app/bin/warmup_apps.sh
    """

    [
      image,
      image_envs,
      hub_config,
      apps_config,
      notebook,
      apps_warmup
    ]
    |> Enum.reject(&is_nil/1)
    |> Enum.join("\n")
  end

  defp format_team_hub_config(config, hub, secrets, file_systems) do
    base_env =
      """
      ARG TEAMS_KEY="#{hub.teams_key}"

      # Teams Hub configuration for airgapped deployment
      ENV LIVEBOOK_TEAMS_KEY ${TEAMS_KEY}
      ENV LIVEBOOK_TEAMS_NAME "#{hub.hub_name}"
      ENV LIVEBOOK_TEAMS_OFFLINE_KEY "#{hub.org_public_key}"
      """

    secrets =
      if secrets != [] do
        """
        ENV LIVEBOOK_TEAMS_SECRETS "#{encrypt_secrets_to_dockerfile(secrets, hub)}"
        """
      end

    file_systems =
      if file_systems != [] do
        """
        ENV LIVEBOOK_TEAMS_FS "#{encrypt_file_systems_to_dockerfile(file_systems, hub)}"
        """
      end

    zta =
      if config.zta_provider && config.zta_key do
        """
        ENV LIVEBOOK_IDENTITY_PROVIDER "#{config.zta_provider}:#{config.zta_key}"
        """
      end

    [base_env, secrets, file_systems, zta]
    |> Enum.reject(&is_nil/1)
    |> Enum.join()
  end

  defp format_personal_hub_config(config, hub, hub_secrets, secrets) do
    secrets = used_secrets(config, hub, secrets, hub_secrets) |> Enum.sort_by(& &1.name)

    if secrets != [] do
      envs = secrets |> Enum.map(&{"LB_" <> &1.name, &1.value}) |> format_envs()

      """
      # Personal Hub secrets
      #{envs}\
      """
    end
  end

  defp format_envs([]), do: nil

  defp format_envs(list) do
    Enum.map_join(list, fn {key, value} -> ~s/ENV #{key} "#{value}"\n/ end)
  end

  defp encrypt_secrets_to_dockerfile(secrets, hub) do
    secrets_map =
      for %{name: name, value: value} <- secrets,
          into: %{},
          do: {name, value}

    encrypt_to_dockerfile(hub, secrets_map)
  end

  defp encrypt_file_systems_to_dockerfile(file_systems, hub) do
    file_systems =
      for file_system <- file_systems do
        file_system
        |> Livebook.FileSystem.dump()
        |> Map.put_new(:type, Livebook.FileSystems.type(file_system))
      end

    encrypt_to_dockerfile(hub, file_systems)
  end

  defp encrypt_to_dockerfile(hub, data) do
    secret_key = Livebook.Teams.derive_key(hub.teams_key)

    data
    |> Jason.encode!()
    |> Livebook.Teams.encrypt(secret_key)
  end

  defp used_secrets(config, hub, secrets, hub_secrets) do
    if config.deploy_all do
      hub_secrets
    else
      for {_, secret} <- secrets, secret.hub_id == hub.id, do: secret
    end
  end
end

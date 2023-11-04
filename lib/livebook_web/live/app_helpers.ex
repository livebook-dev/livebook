defmodule LivebookWeb.AppHelpers do
  use LivebookWeb, :html

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
      <div class="grid grid-cols-1 md:grid-cols-2">
        <.select_field
          label="Clustering"
          help={
            ~S'''
            When running multiple
            instances of Livebook,
            they need to be connected
            into a single cluster.
            You must either deploy
            it as a single instance
            or choose a platform to
            enable clustering on.
            '''
          }
          field={@form[:clustering]}
          options={[
            {"Single instance", ""},
            {"Fly.io", "fly_io"}
          ]}
        />
      </div>
      <%= if Hubs.Provider.type(@hub) == "team" do %>
        <div class="flex flex-col">
          <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
            <.select_field
              label="Zero Trust Authentication provider"
              field={@form[:zta_provider]}
              help={
                ~S'''
                Enable this option if you want
                to deploy your notebooks behind
                an authentication proxy
                '''
              }
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
                   do: {provider.name, provider.type}

  defp zta_options(), do: @zta_options

  @docker_tag_options for image <- Livebook.Config.docker_images(), do: {image.tag, image.name}

  defp docker_tag_options(), do: @docker_tag_options

  @doc """
  Renders Docker deployment instruction for an app.
  """
  attr :hub, :map, required: true
  attr :dockerfile, :string, required: true
  attr :dockerfile_config, :map, required: true

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
        <li :if={@dockerfile_config.clustering} class="flex gap-2">
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
    """
  end

  defp zta_metadata(nil), do: nil

  defp zta_metadata(zta_provider) do
    Enum.find(Livebook.Config.identity_providers(), &(&1.type == zta_provider))
  end

  @doc """
  Updates app list with the given apps event.
  """
  def update_app_list(apps, event)

  def update_app_list(apps, {:app_created, app}) do
    if app in apps, do: apps, else: [app | apps]
  end

  def update_app_list(apps, {:app_updated, app}) do
    Enum.map(apps, fn other ->
      if other.slug == app.slug, do: app, else: other
    end)
  end

  def update_app_list(apps, {:app_closed, app}) do
    Enum.reject(apps, &(&1.slug == app.slug))
  end
end

defmodule LivebookWeb.AppComponents do
  use LivebookWeb, :html

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
    <.app_status_indicator text={@show_label && "Shutting down"} variant="inactive" />
    """
  end

  def app_status(%{status: %{lifecycle: :deactivated}} = assigns) do
    ~H"""
    <.app_status_indicator text={@show_label && "Deactivated"} variant="inactive" />
    """
  end

  def app_status(%{status: %{execution: :executing}} = assigns) do
    ~H"""
    <.app_status_indicator text={@show_label && "Executing"} variant="progressing" />
    """
  end

  def app_status(%{status: %{execution: :executed}} = assigns) do
    ~H"""
    <.app_status_indicator text={@show_label && "Executed"} variant="success" />
    """
  end

  def app_status(%{status: %{execution: :error}} = assigns) do
    ~H"""
    <.app_status_indicator text={@show_label && "Error"} variant="error" />
    """
  end

  def app_status(%{status: %{execution: :interrupted}} = assigns) do
    ~H"""
    <.app_status_indicator text={@show_label && "Interrupted"} variant="waiting" />
    """
  end

  defp app_status_indicator(assigns) do
    ~H"""
    <span class="flex items-center space-x-2">
      <.status_indicator variant={@variant} />
      <span :if={@text}>{@text}</span>
    </span>
    """
  end

  @doc """
  Shows a confirmation modal and closes the app on confirm.
  """
  def confirm_app_termination(socket, app_pid, title \\ "app") do
    on_confirm = fn socket ->
      Livebook.App.close(app_pid)
      socket
    end

    confirm(socket, on_confirm,
      title: "Terminate #{title}",
      description: "All #{title} sessions will be immediately terminated.",
      confirm_text: "Terminate",
      confirm_icon: "delete-bin-6-line"
    )
  end

  @doc """
  Renders form fields for Deployment Group.
  """
  attr :form, Phoenix.HTML.Form, required: true
  attr :hub, :map, required: true
  attr :disabled, :boolean, default: false

  def deployment_group_form_content(assigns) do
    ~H"""
    <div>
      <div class="flex flex-col gap-4">
        <div>
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
              or choose a strategy to
              connect the instances.
              '''
            }
            field={@form[:clustering]}
            options={[
              {"Automatic", "auto"},
              {"DNS", "dns"},
              {"Single instance", ""}
            ]}
            disabled={@disabled}
          />
          <div :if={to_string(@form[:clustering].value) == "dns"} class="mt-1 text-sm">
            See the
            <a
              class="text-blue-600 hover:text-blue-700"
              href="https://hexdocs.pm/livebook/clustering.html"
            >
              Clustering docs
            </a>
            for more information.
          </div>
          <p class="mt-1 text-sm">
            Automatic clustering is available when deploying to Fly.io and Kubernetes.
          </p>
        </div>
      </div>
    </div>

    <%= if Livebook.Hubs.Provider.type(@hub) == "team" and to_string(@form[:mode].value) == "online" do %>
      <div class="flex flex-col gap-2">
        <.checkbox_field
          field={@form[:teams_auth]}
          label="Authenticate via Livebook Teams"
          help={
            ~S'''
            When enabled, apps deployed in
            this deployment group will use
            Livebook Teams for authentication.
            '''
          }
          small
        />
      </div>
    <% end %>
    """
  end

  @doc """
  Lists all docker tag options.
  """
  @docker_tag_options for image <- Livebook.Config.docker_images(), do: {image.tag, image.name}
  def docker_tag_options(), do: @docker_tag_options

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

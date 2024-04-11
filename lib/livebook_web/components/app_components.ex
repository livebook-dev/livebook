defmodule LivebookWeb.AppComponents do
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
    <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
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
        disabled={@disabled}
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
            disabled={@disabled}
          />
          <.text_field
            :if={zta_metadata = zta_metadata(@form[:zta_provider].value)}
            field={@form[:zta_key]}
            label={zta_metadata.value}
            placeholder={zta_placeholder(zta_metadata)}
            phx-debounce
            disabled={@disabled}
          />
        </div>
        <div :if={zta_metadata = zta_metadata(@form[:zta_provider].value)} class="text-sm mt-1">
          See the
          <a
            class="text-blue-800 hover:text-blue-600"
            href={"https://hexdocs.pm/livebook/#{zta_metadata.type}.html"}
          >
            Authentication with <%= zta_metadata.name %> docs
          </a>
          for more information.
        </div>
      </div>
    <% end %>
    """
  end

  @zta_options for provider <- Livebook.Config.identity_providers(),
                   do: {provider.name, provider.type}

  defp zta_options(), do: @zta_options

  defp zta_metadata(nil), do: nil

  defp zta_metadata(zta_provider) do
    Enum.find(Livebook.Config.identity_providers(), &(&1.type == zta_provider))
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

  defp zta_placeholder(%{placeholder: placeholder}), do: placeholder
  defp zta_placeholder(_), do: nil
end

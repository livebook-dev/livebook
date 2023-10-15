defmodule LivebookWeb.SessionLive.AppDockerComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  alias Livebook.Hubs
  alias Livebook.FileSystem

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    {:ok,
     socket
     |> assign(settings_valid?: Livebook.Notebook.AppSettings.valid?(socket.assigns.settings))
     |> assign(
       hub_secrets: Hubs.get_secrets(assigns.hub),
       hub_file_systems: Hubs.get_file_systems(assigns.hub, hub_only: true)
     )
     |> assign_new(:changeset, fn -> LivebookWeb.AppHelpers.docker_config_changeset() end)
     |> assign_new(:save_result, fn -> nil end)
     |> update_dockerfile()}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 max-w-4xl flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        App deployment
      </h3>
      <.content
        file={@file}
        settings_valid?={@settings_valid?}
        hub={@hub}
        hub_secrets={@hub_secrets}
        app_settings={@app_settings}
        hub_file_systems={@hub_file_systems}
        file_entries={@file_entries}
        secrets={@secrets}
        changeset={@changeset}
        session={@session}
        dockerfile={@dockerfile}
        save_result={@save_result}
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
      <.link class="text-blue-600 font-medium" patch={~p"/sessions/#{@session.id}/settings/file"}>
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
      <.link class="text-blue-600 font-medium" patch={~p"/sessions/#{@session.id}/settings/app"}>
        <span>Configure</span>
        <.remix_icon icon="arrow-right-line" />
      </.link>
    </div>
    """
  end

  defp content(assigns) do
    ~H"""
    <div class="flex flex-col gap-4">
      <p class="text-gray-700">
        You can deploy this app in the cloud using Docker. To do that, configure
        the deployment and then use the generated Dockerfile.
      </p>
      <div class="flex flex-col gap-2">
        <.message_box
          :for={
            warning <-
              warnings(@changeset, @hub, @hub_secrets, @app_settings, @file_entries, @secrets)
          }
          kind={:warning}
          message={warning}
        />
      </div>
      <p class="text-gray-700">
        <.label>Hub</.label>
        <span>
          <span class="text-lg"><%= @hub.hub_emoji %></span>
          <span><%= @hub.hub_name %></span>
        </span>
      </p>
      <.form :let={f} for={@changeset} as={:data} phx-change="validate" phx-target={@myself}>
        <LivebookWeb.AppHelpers.docker_config_form_content hub={@hub} form={f} />
      </.form>
      <.save_result :if={@save_result} save_result={@save_result} />
      <LivebookWeb.AppHelpers.docker_instructions hub={@hub} dockerfile={@dockerfile}>
        <:dockerfile_actions>
          <button
            :if={@file}
            class="button-base button-gray whitespace-nowrap py-1 px-2"
            type="button"
            aria-label="save dockerfile alongside the notebook"
            phx-click="save_dockerfile"
            phx-target={@myself}
          >
            <.remix_icon icon="save-line" class="align-middle mr-1 text-xs" />
            <span class="font-normal text-xs">Save alongside notebook</span>
          </button>
        </:dockerfile_actions>
      </LivebookWeb.AppHelpers.docker_instructions>
    </div>
    """
  end

  defp save_result(%{save_result: {:ok, file}}) do
    assigns = %{path: file.path}

    ~H"""
    <.message_box kind={:info} message={"File saved at #{@path}"} />
    """
  end

  defp save_result(%{save_result: {:error, message}}) do
    assigns = %{message: message}

    ~H"""
    <.message_box kind={:error} message={@message} />
    """
  end

  @impl true
  def handle_event("validate", %{"data" => data}, socket) do
    changeset =
      data
      |> LivebookWeb.AppHelpers.docker_config_changeset()
      |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, changeset: changeset) |> update_dockerfile()}
  end

  def handle_event("save_dockerfile", %{}, socket) do
    dockerfile_file = FileSystem.File.resolve(socket.assigns.file, "./Dockerfile")

    save_result =
      case FileSystem.File.write(dockerfile_file, socket.assigns.dockerfile) do
        :ok -> {:ok, dockerfile_file}
        {:error, message} -> {:error, message}
      end

    {:noreply, assign(socket, save_result: save_result)}
  end

  defp update_dockerfile(socket) when socket.assigns.file == nil do
    assign(socket, dockerfile: nil)
  end

  defp update_dockerfile(socket) do
    config = apply_changes(socket.assigns.changeset)

    %{
      hub: hub,
      hub_secrets: hub_secrets,
      hub_file_systems: hub_file_systems,
      file: file,
      file_entries: file_entries,
      secrets: secrets
    } = socket.assigns

    dockerfile =
      LivebookWeb.AppHelpers.build_dockerfile(
        config,
        hub,
        hub_secrets,
        hub_file_systems,
        file,
        file_entries,
        secrets
      )

    assign(socket, :dockerfile, dockerfile)
  end

  defp warnings(changeset, hub, hub_secrets, app_settings, file_entries, secrets) do
    config = apply_changes(changeset)

    common_warnings =
      [
        if Livebook.Session.Data.session_secrets(secrets, hub.id) != [] do
          "The notebook uses session secrets, but those are not available to deployed apps." <>
            " Convert them to Hub secrets instead."
        end
      ]

    hub_warnings =
      case Hubs.Provider.type(hub) do
        "personal" ->
          [
            if used_secrets(config, hub, secrets, hub_secrets) != [] do
              "You are deploying an app with secrets and the secrets are included in the Dockerfile" <>
                " as environment variables. If someone else deploys this app, they must also set the" <>
                " same secrets. Use Livebook Teams to automatically encrypt and synchronize secrets" <>
                " across your team and deployments."
            end,
            if module = find_hub_file_system(file_entries) do
              name = LivebookWeb.FileSystemHelpers.file_system_name(module)

              "The #{name} file storage, defined in your personal hub, will not be available in the Docker image." <>
                " You must either download all references as attachments or use Livebook Teams to automatically" <>
                " encrypt and synchronize file storages across your team and deployments."
            end,
            if app_settings.access_type == :public do
              "This app has no password configuration and anyone with access to the server will be able" <>
                " to use it. Either configure a password or use Livebook Teams to add Zero Trust Authentication" <>
                " to your deployed notebooks."
            end
          ]

        "team" ->
          [
            if app_settings.access_type == :public and
                 (config.zta_provider == nil or config.zta_key == nil) do
              "This app has no password configuration and anyone with access to the server will be able" <>
                " to use it. Either configure a password or configure Zero Trust Authentication."
            end
          ]
      end

    Enum.reject(common_warnings ++ hub_warnings, &is_nil/1)
  end

  defp find_hub_file_system(file_entries) do
    Enum.find_value(file_entries, fn entry ->
      entry.type == :file && entry.file.file_system_module != FileSystem.Local &&
        entry.file.file_system_module
    end)
  end

  defp used_secrets(config, hub, secrets, hub_secrets) do
    if config.deploy_all do
      hub_secrets
    else
      for {_, secret} <- secrets, secret.hub_id == hub.id, do: secret
    end
  end
end

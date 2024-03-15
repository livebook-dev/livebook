defmodule LivebookWeb.SessionLive.AppTeamsComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs.Provider

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)
    deployment_groups = Provider.deployment_groups(assigns.hub)

    deployment_group =
      if assigns.deployment_group_id do
        Enum.find(deployment_groups, &(&1.id == assigns.deployment_group_id))
      end

    app_deployment =
      if deployment_group do
        Enum.find(deployment_group.app_deployments, &(&1.slug == assigns.app_settings.slug))
      end

    socket =
      socket
      |> assign(
        settings_valid?: Livebook.Notebook.AppSettings.valid?(socket.assigns.app_settings),
        app_deployment: app_deployment,
        deployment_groups: deployment_groups,
        deployment_group: deployment_group,
        deployment_group_form: %{"deployment_group_id" => assigns.deployment_group_id},
        deployment_group_id: assigns.deployment_group_id
      )
      |> assign_new(:warnings, fn -> [] end)
      |> assign_new(:message_kind, fn -> :info end)
      |> assign_new(:message, fn -> nil end)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        App deployment with Livebook Teams
      </h3>
      <.content
        file={@file}
        hub={@hub}
        app_deployment={@app_deployment}
        deployment_group={@deployment_group}
        deployment_groups={@deployment_groups}
        deployment_group_form={@deployment_group_form}
        deployment_group_id={@deployment_group_id}
        session={@session}
        warnings={@warnings}
        message_kind={@message_kind}
        message={@message}
        myself={@myself}
      />
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
        patch={~p"/sessions/#{@session.id}/settings/app?context=app-teams"}
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
      <p class="text-gray-700">
        You can deploy this app in the to your own cloud using Livebook Teams. To do that, select
        which deployment group you want to send this app and then click the button to Deploy.
      </p>

      <div class="flex gap-12">
        <p class="text-gray-700">
          <.label>Hub</.label>
          <span>
            <span class="text-lg"><%= @hub.hub_emoji %></span>
            <span><%= @hub.hub_name %></span>
          </span>
        </p>
        <%= if @deployment_groups do %>
          <%= if @deployment_groups != [] do %>
            <.form
              for={@deployment_group_form}
              phx-change="select_deployment_group"
              phx-target={@myself}
              id="select_deployment_group_form"
            >
              <.select_field
                help={deployment_group_help()}
                field={@deployment_group_form[:deployment_group_id]}
                options={deployment_group_options(@deployment_groups)}
                label="Deployment Group"
                name="deployment_group_id"
                value={@deployment_group_id}
              />
            </.form>
          <% else %>
            <p class="text-gray-700">
              <.label help={deployment_group_help()}>
                Deployment Group
              </.label>
              <span>No deployment groups available</span>
            </p>
          <% end %>
        <% end %>
      </div>

      <div :if={@warnings != []} class="flex flex-col gap-2">
        <.message_box :for={warning <- @warnings} kind={:warning}>
          <%= raw(warning) %>
        </.message_box>
      </div>

      <.message_box :if={@message} kind={@message_kind} message={@message} />

      <div :if={@app_deployment} class="space-y-3 pb-4">
        <p class="text-gray-700">Current deployed version:</p>

        <ul class="text-gray-700 space-y-3">
          <li class="flex gap-2">
            <div class="font-bold">Title:</div>
            <span><%= @app_deployment.title %></span>
          </li>
          <li class="flex gap-2">
            <div class="font-bold">Deployed by:</div>
            <span><%= @app_deployment.deployed_by %></span>
          </li>
          <li class="flex gap-2">
            <div class="font-bold">Deployed at:</div>
            <span><%= @app_deployment.deployed_at %></span>
          </li>
        </ul>
      </div>

      <.button
        id="deploy-livebook-agent-button"
        color="blue"
        phx-click="deploy_app"
        phx-target={@myself}
      >
        <.remix_icon icon="rocket-line" /> Deploy to Livebook Agent
      </.button>
    </div>
    """
  end

  @impl true
  def handle_event("select_deployment_group", %{"deployment_group_id" => id}, socket) do
    id = if(id != "", do: id)
    Livebook.Session.set_notebook_deployment_group(socket.assigns.session.pid, id)

    {:noreply, socket}
  end

  def handle_event("deploy_app", _, socket) do
    notebook = Livebook.Session.get_notebook(socket.assigns.session.pid)
    filename = Livebook.Session.file_name_for_download(socket.assigns.session)

    case Livebook.Teams.deploy_app(
           socket.assigns.hub,
           notebook,
           filename,
           socket.assigns.session.files_dir
         ) do
      :ok ->
        deployment_groups = Provider.deployment_groups(socket.assigns.hub)
        deployment_group = Enum.find(deployment_groups, &(&1.id == notebook.deployment_group_id))

        app_deployment =
          Enum.find(deployment_group.app_deployments, &(&1.slug == notebook.app_settings.slug))

        message =
          "App deployment for #{app_deployment.slug} with title #{app_deployment.title} created successfully"

        {:noreply, assign(socket, message_kind: :info, message: message, warnings: [])}

      {:warning, warnings} ->
        {:noreply, assign(socket, warnings: warnings)}

      {:error, error} ->
        {:noreply, assign(socket, message_kind: :error, message: error, warnings: [])}

      {:transport_error, error} ->
        {:noreply, assign(socket, message_kind: :error, message: error, warnings: [])}
    end
  end

  defp deployment_group_options(deployment_groups) do
    for deployment_group <- [%{name: "none", id: nil}] ++ deployment_groups,
        do: {deployment_group.name, deployment_group.id}
  end

  defp deployment_group_help() do
    "Share deployment credentials, secrets, and configuration with deployment groups."
  end
end

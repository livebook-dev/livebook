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
      |> assign_new(:messages, fn -> [] end)

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
        messages={@messages}
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

      <div :if={@messages != []} class="flex flex-col gap-2">
        <.message_box :for={{kind, message} <- @messages} kind={kind}>
          <%= raw(message) %>
        </.message_box>
      </div>

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
    with {:ok, app_deployment} <- pack_app(socket),
         :ok <- deploy_app(socket, app_deployment) do
      message =
        "App deployment for #{app_deployment.slug} with title #{app_deployment.title} created successfully"

      {:noreply, assign(socket, messages: [{:info, message}])}
    end
  end

  defp pack_app(socket) do
    notebook = Livebook.Session.get_notebook(socket.assigns.session.pid)
    files_dir = socket.assigns.session.files_dir

    case Livebook.Teams.AppDeployment.new(notebook, files_dir) do
      {:ok, app_deployment} ->
        {:ok, app_deployment}

      {:warning, warnings} ->
        messages = Enum.map(warnings, &{:error, &1})
        {:noreply, assign(socket, messages: messages)}

      {:error, error} ->
        error = "Failed to pack files: #{error}"
        {:noreply, assign(socket, messages: [{:error, error}])}
    end
  end

  defp deploy_app(socket, app_deployment) do
    app_deployment = Map.replace!(app_deployment, :slug, "@bc")

    case Livebook.Teams.deploy_app(socket.assigns.hub, app_deployment) do
      :ok ->
        :ok

      {:error, %{errors: errors}} ->
        errors = Enum.map(errors, fn {key, error} -> "#{key}: #{normalize_error(error)}" end)
        {:noreply, assign(socket, messages: errors)}

      {:transport_error, error} ->
        {:noreply, assign(socket, messages: [{:error, error}])}
    end
  end

  defp normalize_error({msg, opts}) do
    Enum.reduce(opts, msg, fn {key, value}, acc ->
      String.replace(acc, "%{#{key}}", to_string(value))
    end)
  end

  defp deployment_group_options(deployment_groups) do
    for deployment_group <- [%{name: "none", id: nil}] ++ deployment_groups,
        do: {deployment_group.name, deployment_group.id}
  end

  defp deployment_group_help() do
    "Share deployment credentials, secrets, and configuration with deployment groups."
  end
end

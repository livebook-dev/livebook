defmodule LivebookWeb.SessionLive.AppTeamsLive do
  use LivebookWeb, :live_view

  # We use a child LV, because we want to subscribe and react to many
  # hub-specific events, but they are only relevant while this view
  # is open.
  #
  # We also use the :action assign (similar to :live_action), instead
  # of relaying on URL changes. The main reason for this being, that
  # the initial action is conditional and we couldn't patch the URL
  # on mount.

  alias Livebook.Session
  alias Livebook.Teams

  @impl true
  def mount(_params, %{"session_pid" => session_pid}, socket) do
    session = Session.get_by_pid(session_pid)

    %{
      hub_id: hub_id,
      app_settings: app_settings,
      deployment_group_id: deployment_group_id
    } = Session.get_notebook(session_pid)

    hub = Livebook.Hubs.fetch_hub!(hub_id)

    if connected?(socket) do
      Session.subscribe(session.id)

      Teams.Broadcasts.subscribe([:deployment_groups, :app_deployments, :agents])
    end

    socket =
      socket
      |> assign(
        session: session,
        hub: hub,
        slug: app_settings.slug,
        settings_valid?: Livebook.Notebook.AppSettings.valid?(app_settings),
        messages: [],
        action: :deployment_groups
      )
      |> assign_deployment_groups()
      |> assign_app_deployments()
      |> assign_agents()
      |> assign_deployment_group(deployment_group_id)
      |> assign_app_deployment()
      |> assign_initial()
      |> navigate_if_no_deployment_groups()

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-8">
      <div class="flex flex-col gap-1">
        <h3 class="text-2xl font-semibold text-gray-800">
          App deployment with Livebook Teams
        </h3>
        <h4 :if={subtitle = subtitle(@action)} class="text-gray-600">
          <.remix_icon icon="corner-down-right-line" /> <%= subtitle %>
        </h4>
      </div>

      <div :if={@messages != []} class="flex flex-col gap-2">
        <.message_box :for={{kind, message} <- @messages} kind={kind} message={message} />
      </div>

      <.content
        hub={@hub}
        settings_valid?={@settings_valid?}
        app_deployment={@app_deployment}
        deployment_groups={@deployment_groups}
        num_agents={@num_agents}
        num_app_deployments={@num_app_deployments}
        deployment_group={@deployment_group}
        session={@session}
        messages={@messages}
        action={@action}
        initial?={@initial?}
      />
    </div>
    """
  end

  defp subtitle(:add_deployment_group), do: "Step: add deployment group"
  defp subtitle(:add_agent), do: "Step: add app server"
  defp subtitle(:success), do: "Step: summary"
  defp subtitle(_), do: nil

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

  defp content(%{action: :add_deployment_group} = assigns) do
    ~H"""
    <div class="flex flex-col gap-8">
      <.message_box kind={:info}>
        You must create a deployment group before deploying the app.
      </.message_box>
      <.live_component
        module={LivebookWeb.Hub.Teams.DeploymentGroupFormComponent}
        id="add-deployment-group"
        session={@session}
        hub={@hub}
        return_to={nil}
        force_mode={:online}
        hide_title
      />
    </div>
    """
  end

  defp content(%{action: :add_agent} = assigns) do
    ~H"""
    <div class="flex flex-col gap-8">
      <.message_box :if={@initial?} kind={:info}>
        You must set up an app server for the app to run on.
      </.message_box>
      <div>
        <.live_component
          module={LivebookWeb.Hub.Teams.DeploymentGroupAgentComponent}
          id="add-agent"
          session={@session}
          hub={@hub}
          deployment_group={@deployment_group}
          hide_title
        />
        <div class="mt-6 pt-6 border-t border-gray-200 flex flex-col gap-4">
          <h4 class="text-lg font-semibold text-gray-800">
            Status
          </h4>
          <%= if @num_agents[@deployment_group.id] do %>
            <.message_box kind={:success}>
              An app server is running, click "Deploy" to ship the app!
            </.message_box>
          <% else %>
            <.message_box kind={:info}>
              Awaiting an app server to be set up. If you click "Deploy anyway",
              the app will only start once there is an app server.
            </.message_box>
          <% end %>
          <div class="flex gap-2">
            <%= if @num_agents[@deployment_group.id] do %>
              <.button color="blue" phx-click="deploy_app">
                <.remix_icon icon="rocket-line" /> Deploy
              </.button>
            <% else %>
              <.button color="blue" outlined phx-click="deploy_app">
                <.remix_icon icon="rocket-line" /> Deploy anyway
              </.button>
            <% end %>
            <.button color="gray" outlined phx-click="go_deployment_groups">
              See deployment groups
            </.button>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp content(%{action: :deployment_groups} = assigns) do
    ~H"""
    <div class="flex flex-col gap-6">
      <p class="text-gray-700">
        Deploy this app to your cloud infrastructure using the <.workspace hub={@hub} /> workspace.
      </p>

      <%= if @deployment_group do %>
        <div class="flex flex-col gap-2">
          <div class="flex justify-between items-center">
            <p class="text-gray-700">
              Deploying to:
            </p>
            <button class="font-medium text-blue-600" phx-click="unselect_deployment_group">
              Change
            </button>
          </div>
          <div class="flex flex-col gap-3">
            <.deployment_group_entry
              deployment_group={@deployment_group}
              num_agents={@num_agents}
              num_app_deployments={@num_app_deployments}
              active
            />
          </div>
        </div>

        <div :if={@app_deployment} class="space-y-3">
          <p class="text-gray-700">Current version:</p>
          <.app_deployment_card app_deployment={@app_deployment} deployment_group={@deployment_group} />
        </div>

        <.message_box :if={@num_agents[@deployment_group.id] == nil} kind={:warning}>
          The selected deployment group has no app servers. If you click "Deploy anyway",
          the app will only start once there is an app server.
        </.message_box>

        <%= if @num_agents[@deployment_group.id] do %>
          <div>
            <.button color="blue" phx-click="deploy_app">
              <.remix_icon icon="rocket-line" /> Deploy
            </.button>
          </div>
        <% else %>
          <div>
            <.button color="blue" phx-click="go_add_agent">
              <.remix_icon icon="add-line" /> Add app server
            </.button>
            <.button color="blue" outlined phx-click="deploy_app">
              <.remix_icon icon="rocket-line" /> Deploy anyway
            </.button>
          </div>
        <% end %>
      <% else %>
        <.no_entries :if={@deployment_groups == []}>
          No online deployment groups yet.
          <.link
            class="font-medium text-blue-600"
            patch={~p"/sessions/#{@session.id}/add-deployment-group"}
          >
            Add deployment group <.remix_icon icon="arrow-right-line" class="align-middle" />
          </.link>
        </.no_entries>

        <div :if={@deployment_groups != []} class="flex flex-col gap-2">
          <p class="text-gray-700">
            Choose a deployment group:
          </p>
          <div class="flex flex-col gap-3">
            <.deployment_group_entry
              :for={deployment_group <- @deployment_groups}
              deployment_group={deployment_group}
              num_agents={@num_agents}
              num_app_deployments={@num_app_deployments}
              selectable
            />
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  defp content(%{action: :success} = assigns) do
    ~H"""
    <div class="flex flex-col gap-8">
      <.message_box kind={:success}>
        <div class="flex items-center justify-between">
          <span>App deployment created successfully.</span>

          <.link
            href={"#{Livebook.Config.teams_url()}/orgs/#{@hub.org_id}"}
            target="_blank"
            class="font-medium text-blue-600"
          >
            <span>See all deployed apps</span>
            <.remix_icon icon="external-link-line" />
          </.link>
        </div>
      </.message_box>
      <.app_deployment_card
        :if={@app_deployment}
        app_deployment={@app_deployment}
        deployment_group={@deployment_group}
      />
      <div>
        <.button color="gray" outlined phx-click="go_deployment_groups">
          See deployment groups
        </.button>
      </div>
    </div>
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

  attr :active, :boolean, default: false
  attr :selectable, :boolean, default: false
  attr :deployment_group, :map, required: true
  attr :num_agents, :map, required: true
  attr :num_app_deployments, :map, required: true
  attr :rest, :global

  defp deployment_group_entry(assigns) do
    ~H"""
    <div
      class={[
        "border p-3 rounded-lg",
        @selectable && "cursor-pointer",
        if(@active,
          do: "border-blue-600 bg-blue-50",
          else: "border-gray-200"
        )
      ]}
      phx-click={@selectable && "select_deployment_group"}
      phx-value-id={@deployment_group.id}
      {@rest}
    >
      <div class="flex justify-between items-center">
        <div class="flex gap-2 items-center text-gray-700">
          <h3 class="text-sm">
            <span class="font-semibold"><%= @deployment_group.name %></span>
            <span :if={url = @deployment_group.url}>(<%= url %>)</span>
          </h3>
        </div>
        <div class="flex gap-2">
          <div class="text-sm text-gray-700 border-l border-gray-300 pl-2">
            App servers: <%= @num_agents[@deployment_group.id] || 0 %>
          </div>
          <div class="text-sm text-gray-700 border-l border-gray-300 pl-2">
            Apps deployed: <%= @num_app_deployments[@deployment_group.id] || 0 %>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp app_deployment_card(assigns) do
    ~H"""
    <div class="flex gap-4 sm:gap-12 border border-gray-200 rounded-lg p-4">
      <.labeled_text label="Slug">
        <%= if @deployment_group.url do %>
          <.link
            href={@deployment_group.url <> ~p"/apps/#{@app_deployment.slug}"}
            target="_blank"
            class="text-blue-600 font-medium"
          >
            /<%= @app_deployment.slug %>
          </.link>
        <% else %>
          <span>
            /<%= @app_deployment.slug %>
          </span>
        <% end %>
      </.labeled_text>
      <.labeled_text label="Title">
        <%= @app_deployment.title %>
      </.labeled_text>
      <.labeled_text label="Deployed by">
        <%= @app_deployment.deployed_by %>
      </.labeled_text>
      <.labeled_text label="Deployed">
        <%= LivebookWeb.HTMLHelpers.format_datetime_relatively(@app_deployment.deployed_at) %> ago
      </.labeled_text>
    </div>
    """
  end

  @impl true
  def handle_event("unselect_deployment_group", %{}, socket) do
    Livebook.Session.set_notebook_deployment_group(socket.assigns.session.pid, nil)
    {:noreply, socket}
  end

  def handle_event("select_deployment_group", %{"id" => id}, socket) do
    Livebook.Session.set_notebook_deployment_group(socket.assigns.session.pid, id)
    {:noreply, socket}
  end

  def handle_event("deploy_app", _, socket) do
    with {:ok, app_deployment} <- pack_app(socket),
         :ok <- deploy_app(socket, app_deployment) do
      {:noreply, navigate(socket, :success)}
    end
  end

  def handle_event("go_add_agent", %{}, socket) do
    {:noreply, navigate(socket, :add_agent)}
  end

  def handle_event("go_deployment_groups", %{}, socket) do
    {:noreply, navigate(socket, :deployment_groups)}
  end

  @impl true
  def handle_info({event, deployment_group}, socket)
      when event in [
             :deployment_group_created,
             :deployment_group_update,
             :deployment_group_deleted
           ] and deployment_group.hub_id == socket.assigns.hub.id do
    current_deployment_group_id =
      if current_deployment_group = socket.assigns.deployment_group do
        current_deployment_group.id
      end

    {socket, deployment_group_id} =
      if socket.assigns.initial? and event == :deployment_group_created do
        Livebook.Session.set_notebook_deployment_group(
          socket.assigns.session.pid,
          deployment_group.id
        )

        {navigate(socket, :add_agent), deployment_group.id}
      else
        {socket, current_deployment_group_id}
      end

    {:noreply,
     socket
     |> assign_deployment_groups()
     |> assign_deployment_group(deployment_group_id)
     |> navigate_if_no_deployment_groups()}
  end

  def handle_info({event, agent}, socket)
      when event in [:agent_joined, :agent_left] and agent.hub_id == socket.assigns.hub.id do
    {:noreply, assign_agents(socket)}
  end

  def handle_info({event, app_deployment}, socket)
      when event in [:app_deployment_started, :app_deployment_stopped] and
             app_deployment.hub_id == socket.assigns.hub.id do
    {:noreply, socket |> assign_app_deployments() |> assign_app_deployment()}
  end

  def handle_info(
        {:operation, {:set_notebook_deployment_group, _client_id, deployment_group_id}},
        socket
      ) do
    {:noreply,
     socket
     |> assign_deployment_group(deployment_group_id)
     |> assign_app_deployment()}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp assign_deployment_groups(socket) do
    deployment_groups =
      socket.assigns.hub
      |> Teams.get_deployment_groups()
      |> Enum.filter(&(&1.mode == :online))
      |> Enum.sort_by(& &1.name)

    assign(socket, deployment_groups: deployment_groups)
  end

  defp assign_app_deployments(socket) do
    app_deployments = Teams.get_app_deployments(socket.assigns.hub)
    num_app_deployments = Enum.frequencies_by(app_deployments, & &1.deployment_group_id)
    assign(socket, app_deployments: app_deployments, num_app_deployments: num_app_deployments)
  end

  defp assign_agents(socket) do
    agents = Teams.get_agents(socket.assigns.hub)
    num_agents = Enum.frequencies_by(agents, & &1.deployment_group_id)
    assign(socket, num_agents: num_agents)
  end

  defp assign_deployment_group(socket, deployment_group_id) do
    deployment_group =
      if deployment_group_id do
        Enum.find(socket.assigns.deployment_groups, &(&1.id == deployment_group_id))
      end

    assign(socket, deployment_group: deployment_group)
  end

  defp assign_app_deployment(socket) do
    app_deployment =
      if deployment_group = socket.assigns.deployment_group do
        Enum.find(
          socket.assigns.app_deployments,
          &(&1.slug == socket.assigns.slug and &1.deployment_group_id == deployment_group.id)
        )
      end

    assign(socket, app_deployment: app_deployment)
  end

  defp assign_initial(socket) do
    assign(socket, initial?: socket.assigns.deployment_groups == [])
  end

  defp navigate(socket, action)
       when action in [:deployment_groups, :add_deployment_group, :add_agent, :success] do
    assign(socket, action: action, messages: [])
  end

  defp navigate_if_no_deployment_groups(socket) do
    if socket.assigns.deployment_groups == [] do
      navigate(socket, :add_deployment_group)
    else
      socket
    end
  end

  defp pack_app(socket) do
    notebook = Livebook.Session.get_notebook(socket.assigns.session.pid)
    files_dir = socket.assigns.session.files_dir

    case Teams.AppDeployment.new(notebook, files_dir) do
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
    case Teams.deploy_app(socket.assigns.hub, app_deployment) do
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
end

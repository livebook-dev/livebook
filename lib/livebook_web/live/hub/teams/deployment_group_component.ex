defmodule LivebookWeb.Hub.Teams.DeploymentGroupComponent do
  use LivebookWeb, :live_component

  alias LivebookWeb.NotFoundError
  alias Livebook.Teams.DeploymentGroup

  @impl true
  def update(assigns, socket) do
    deployment_group = assigns.deployment_group
    secret_name = assigns.params["secret_name"]

    secret_value =
      if assigns.params["deployment_group_id"] == deployment_group.id and
           assigns.live_action == :edit_deployment_group_secret do
        Enum.find_value(deployment_group.secrets, &(&1.name == secret_name and &1.value)) ||
          raise(NotFoundError, "could not find secret matching #{inspect(secret_name)}")
      end

    {:ok,
     socket
     |> assign(assigns)
     |> assign(secret_name: secret_name, secret_value: secret_value)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id}>
      <div class="px-5 py-4 flex flex-col rounded-lg border border-gray-900/10">
        <div class="flex justify-between items-start">
          <div>
            <div class="flex gap-2 items-center text-gray-700">
              <h3 class="font-semibold"><%= @deployment_group.name %></h3>
              <%= if @deployment_group.mode == :online do %>
                <div class="bg-green-100 text-green-800 text-xs px-2.5 py-0.5 rounded cursor-default">
                  Online
                </div>
              <% else %>
                <div class="bg-gray-200 text-gray-800 text-xs px-2.5 py-0.5 rounded cursor-default">
                  Airgapped
                </div>
              <% end %>
            </div>
            <.link
              :if={@deployment_group.url}
              href={@deployment_group.url}
              class="text-xs font-medium text-blue-600 mt-1"
              target="_blank"
            >
              <%= DeploymentGroup.url_without_scheme(@deployment_group) %>
            </.link>
          </div>
          <.link
            href={Livebook.Config.teams_url() <> "/orgs/#{@hub.org_id}/deployments/groups/#{@deployment_group.id}"}
            class="text-sm font-medium text-blue-600"
            target="_blank"
          >
            <.remix_icon icon="external-link-line" /> Edit on Teams
          </.link>
        </div>
        <!-- Overview -->
        <div :if={@deployment_group.mode == :online} class="flex flex-col lg:flex-row justify-center">
          <.labeled_text class="grow mt-6 lg:border-l border-gray-200 lg:pl-4" label="App servers">
            <span class="text-lg font-normal" aria-label="app servers">
              <%= @agents_count %>
            </span>
            <.link
              patch={~p"/hub/#{@hub.id}/groups/#{@deployment_group.id}/agents/new"}
              class="pl-2 text-blue-600 font-medium"
            >
              + Deploy
            </.link>
          </.labeled_text>
          <.labeled_text class="grow mt-6 lg:border-l border-gray-200 lg:pl-4" label="Apps deployed">
            <span class="text-lg font-normal" aria-label="apps deployed">
              <%= @app_deployments_count %>
            </span>
            <.link
              patch={~p"/hub/#{@hub.id}/groups/#{@deployment_group.id}/apps/new"}
              class="pl-2 text-blue-600 font-medium"
            >
              + Add new
            </.link>
          </.labeled_text>
          <.labeled_text class="grow mt-6 lg:border-l border-gray-200 lg:pl-4" label="Authentication">
            <span class="text-lg font-normal">
              <%= Livebook.ZTA.provider_name(@deployment_group.zta_provider) %>
            </span>
          </.labeled_text>
        </div>
        <!-- Aditional Secrets -->
        <div id={"secrets-group-#{@deployment_group.id}"} class="flex flex-col space-y-4 mt-6">
          <a
            href="#"
            class="-ml-1 flex flex-row text-gray-500 text-sm"
            phx-click={JS.toggle(to: "#secrets-group-#{@deployment_group.id} [data-toggle]")}
          >
            <.remix_icon data-toggle icon="arrow-right-s-line" />
            <.remix_icon data-toggle icon="arrow-down-s-line hidden" />
            <span class="pl-1">Additional secrets</span>
          </a>
          <div data-toggle class="hidden text-sm flex space-y-4">
            <div class="flex flex-row space-x-2">
              <p class="grow text-gray-700">
                Secrets that are exclusive to apps deployed to this group.
                In case of conflicts, these secrets take precedence over Workspace secrets.
              </p>

              <div class="self-center">
                <.button
                  aria-label="add secret"
                  outlined
                  patch={~p"/hub/#{@hub.id}/groups/#{@deployment_group.id}/secrets/new"}
                >
                  Add secret
                </.button>
              </div>
            </div>

            <.live_component
              module={LivebookWeb.Hub.SecretListComponent}
              id={"dg-secrets-list-#{@deployment_group.id}"}
              hub={@hub}
              secrets={@deployment_group.secrets}
              edit_path={"hub/#{@hub.id}/groups/#{@deployment_group.id}/secrets/edit"}
              return_to={~p"/hub/#{@hub.id}"}
            />
          </div>
        </div>
      </div>

      <%= if @params["deployment_group_id"] == @deployment_group.id do %>
        <.modal
          :if={@live_action in [:new_deployment_group_secret, :edit_deployment_group_secret]}
          id="deployment-group-secrets-modal"
          show
          width={:medium}
          patch={~p"/hub/#{@hub.id}"}
        >
          <.live_component
            module={LivebookWeb.Hub.SecretFormComponent}
            id="deployment-group-secrets"
            hub={@hub}
            deployment_group_id={@deployment_group.id}
            secret_name={@secret_name}
            secret_value={@secret_value}
            return_to={~p"/hub/#{@hub.id}"}
          />
        </.modal>

        <.modal
          :if={@live_action == :new_deployment_group_agent}
          id="deployment-group-agent-modal"
          show
          width={:big}
          patch={~p"/hub/#{@hub.id}"}
        >
          <.live_component
            module={LivebookWeb.Hub.Teams.DeploymentGroupAgentComponent}
            id="deployment-group-agent-instance"
            hub={@hub}
            deployment_group={@deployment_group}
            return_to={nil}
          />
        </.modal>

        <.modal
          :if={@live_action == :new_deployment_group_app}
          id="deployment-group-app-modal"
          show
          width={:medium}
          patch={~p"/hub/#{@hub.id}"}
        >
          <div class="flex flex-col space-y-3">
            <h3 class="text-2xl font-semibold text-gray-800">
              New app deployment
            </h3>

            <p class="text-gray-700">
              To deploy a new app, open up a notebook of your choice and click
              on the <.remix_icon icon="rocket-line" /> icon on the sidebar. Follow
              the steps there to deploy with Livebook Teams.
            </p>
          </div>
        </.modal>
      <% end %>
    </div>
    """
  end
end

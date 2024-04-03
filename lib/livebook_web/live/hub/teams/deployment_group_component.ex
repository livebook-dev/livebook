defmodule LivebookWeb.Hub.Teams.DeploymentGroupComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs
  alias LivebookWeb.NotFoundError

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
                  <%= @deployment_group.mode %>
                </div>
              <% else %>
                <div class="bg-red-100 text-red-800 text-xs px-2.5 py-0.5 rounded cursor-default">
                  <%= @deployment_group.mode %>
                </div>
              <% end %>
            </div>
            <div class="text-sm mt-1">internal-domain.example.com</div>
          </div>
          <.link
            href={Livebook.Config.teams_url() <> "/orgs/#{@hub.org_id}/deployments/groups/#{@deployment_group.id}"}
            class="text-sm font-semibold text-blue-600  hover:text-blue-700"
            target="_blank"
          >
            <.remix_icon icon="external-link-line" /> Edit on Teams
          </.link>
        </div>
        <!-- Overview -->
        <div :if={@deployment_group.mode == :online} class="flex flex-col lg:flex-row justify-center">
          <.labeled_text class="grow mt-6 lg:border-l lg:pl-4" label="Instances running">
            <span class="text-lg font-normal">-1</span>
            <.link
              patch={~p"/hub/#{@hub.id}/groups/#{@deployment_group.id}/agents/new"}
              class="pl-2 text-blue-600"
            >
              + Add new
            </.link>
          </.labeled_text>
          <.labeled_text class="grow mt-6 lg:border-l lg:pl-4" label="Apps deployed">
            <span class="text-lg font-normal"><%= @app_deployments_count %></span>
            <.link
              patch={~p"/hub/#{@hub.id}/groups/#{@deployment_group.id}/apps/new"}
              class="pl-2 text-blue-600"
            >
              + Add new
            </.link>
          </.labeled_text>
          <.labeled_text class="grow mt-6 lg:border-l lg:pl-4" label="Authentication">
            <span class="text-lg font-normal">
              <%= String.capitalize(to_string(@deployment_group.zta_provider || "none")) %>
            </span>
          </.labeled_text>
        </div>
        <!-- Aditional Secrets -->
        <div id={"secrets-group-#{@deployment_group.id}"} class="flex flex-col space-y-4 mt-6">
          <a
            href="#"
            class="-ml-1 flex flex-row text-gray-500 text-sm"
            phx-click={JS.toggle(to: "#secrets-group-#{@deployment_group.id} [data-toggler]")}
          >
            <.remix_icon data-toggler icon="arrow-right-s-line" />
            <.remix_icon data-toggler icon="arrow-down-s-line hidden" />
            <span class="pl-1">Additional secrets</span>
          </a>
          <div data-toggler class="hidden text-sm flex space-y-4">
            <div class="flex flex-row space-x-2">
              <p class="grow text-gray-700">
                Secrets that are exclusive to apps deployed to this group.
                In case of conflicts, these secrets take precedence over Hub secrets.
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
          <div class="p-6 max-w-4xl flex flex-col space-y-3">
            <h3 class="text-2xl font-semibold text-gray-800">
              Deployment group instance setup
            </h3>

            <p class="text-gray-700">
              A deployment group instance is an instance of Livebook where you can
              deploy Livebook apps via Livebook Teams.
            </p>

            <.table id="hub-agent-keys-table" rows={@deployment_group.agent_keys}>
              <:col :let={agent_key} label="ID"><%= agent_key.id %></:col>
              <:col :let={agent_key} label="Key">
                <div class="flex flex-nowrap gap-2">
                  <div class="grow">
                    <.password_field
                      id={"agent-key-#{agent_key.id}"}
                      name="agent_key"
                      value={agent_key.key}
                      readonly
                    />
                  </div>

                  <span
                    data-tooltip="Copied to clipboard"
                    aria-label="copy to clipboard"
                    phx-click={
                      JS.dispatch("lb:clipcopy", to: "#agent-key-#{agent_key.id}")
                      |> JS.transition("tooltip top", time: 2000)
                    }
                  >
                    <.button color="gray" small type="button">
                      <.remix_icon icon="clipboard-line" class="text-xl leading-none py-1" />
                    </.button>
                  </span>
                </div>
              </:col>
            </.table>

            <p class="text-gray-700">
              Use the Dockerfile below to set up an instance in your own infrastructure.
              Once the instance is running, it will connect to Livebook Teams and become
              available for app deployments.
            </p>

            <div class="flex flex-col gap-4">
              <div>
                <div class="flex items-end mb-1 gap-1">
                  <span class="text-sm text-gray-700 font-semibold">Dockerfile</span>
                  <div class="grow" />
                  <.button
                    color="gray"
                    small
                    data-tooltip="Copied to clipboard"
                    type="button"
                    aria-label="copy to clipboard"
                    phx-click={
                      JS.dispatch("lb:clipcopy", to: "#agent-dockerfile-source")
                      |> JS.add_class("", transition: {"tooltip top", "", ""}, time: 2000)
                    }
                  >
                    <.remix_icon icon="clipboard-line" />
                    <span>Copy source</span>
                  </.button>
                </div>

                <.code_preview
                  source_id="agent-dockerfile-source"
                  source={Hubs.Dockerfile.build_agent_dockerfile(Hubs.Dockerfile.config_new(), @hub)}
                  language="dockerfile"
                />
              </div>
            </div>
          </div>
        </.modal>

        <.modal
          :if={@live_action == :new_deployment_group_app}
          id="deployment-group-app-modal"
          show
          width={:medium}
          patch={~p"/hub/#{@hub.id}"}
        >
          <div class="p-6 max-w-4xl flex flex-col space-y-3">
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

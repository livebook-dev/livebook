defmodule LivebookWeb.Hub.Teams.DeploymentGroupListComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="flex flex-col space-y-4">
      <div class="flex flex-col space-y-4">
        <.no_entries :if={@deployment_groups == []}>
          No deployment groups here... yet!
        </.no_entries>
        <div :for={deployment_group <- @deployment_groups} class="flex flex-col space-y-4">
          <div class="bg-white py-6 px-4 flex flex-col space-y-8 rounded-lg border border-gray-900/10">
            <div class="flex justify-between items-start">
              <div>
                <div class="flex gap-4 items-center text-gray-700">
                  <h3 class="font-semibold"><%= deployment_group.name %></h3>

                  <%= if deployment_group.mode == :online do %>
                    <div class="bg-green-100 text-green-800 text-xs px-2.5 py-0.5 rounded cursor-default">
                      <%= deployment_group.mode %>
                    </div>
                  <% else %>
                    <div class="bg-red-100 text-red-800 text-xs px-2.5 py-0.5 rounded cursor-default">
                      <%= deployment_group.mode %>
                    </div>
                  <% end %>
                </div>
                <div class="text-xs mt-1">internal-domain.example.com</div>
              </div>
              <button type="button" class="text-sm font-semibold text-blue-600  hover:text-blue-700 ">
                <.remix_icon icon="links-line" /> Manage on teams
              </button>
            </div>

            <div class="grid grid-cols-3 gap-px bg-white ">
              <div class="bg-gray-50 px-4 py-4 sm:px-6 lg:px-8 rounded-l-lg">
                <p class="text-sm font-medium leading-6 text-gray-500">Instances running</p>
                <p class="mt-2 flex items-baseline gap-x-5">
                  <span class="text-2xl font-semibold tracking-tight text-gray-900">3</span>
                  <span class="text-sm font-semibold text-gray-800">
                    + add new
                  </span>
                </p>
              </div>
              <div class="bg-gray-50 px-4 py-4 sm:px-6 lg:px-8">
                <p class="text-sm font-medium leading-6 text-gray-500">Apps running</p>
                <p class="mt-2 flex items-baseline gap-x-5">
                  <span class="text-2xl font-semibold tracking-tight text-gray-900">0</span>
                  <span class="text-sm font-semibold text-gray-800">+ add new</span>
                </p>
              </div>
              <div class="bg-gray-50 px-4 py-4 sm:px-6 lg:px-8 rounded-r-lg">
                <p class="text-sm font-medium leading-6 text-gray-500">Secrets</p>
                <p class="mt-2 flex items-baseline gap-x-5">
                  <span class="text-2xl font-semibold tracking-tight text-gray-900">0</span>
                  <span class="text-sm font-semibold text-gray-800">+ add new</span>
                </p>
              </div>
            </div>
            <div
              id={"accordion-collapse-#{deployment_group.id}"}
              data-accordion="collapse"
              class="border border-gray-900/10 rounded-md"
            >
              <button
                type="button"
                class="flex items-center justify-between w-full py-3 px-5 rounded-md bg-white text-sm font-semibold text-gray-900 hover:bg-gray-50 focus:z-10 focus:bg-gray-50"
                aria-expanded="true"
                phx-click={
                  JS.toggle(to: "#accordion-collapse-body-#{deployment_group.id}")
                  |> JS.toggle(to: "#arrow-down-#{deployment_group.id}")
                  |> JS.toggle(to: "#arrow-up-#{deployment_group.id}")
                }
                phx-click-away={
                  JS.hide(to: "#accordion-collapse-body-#{deployment_group.id}")
                  |> JS.hide(to: "#arrow-up-#{deployment_group.id}")
                  |> JS.show(to: "#arrow-down-#{deployment_group.id}")
                }
              >
                <span>Secrets</span>
                <.remix_icon
                  id={"arrow-up-#{deployment_group.id}"}
                  icon="arrow-up-s-line"
                  class="hidden"
                />
                <.remix_icon
                  id={"arrow-down-#{deployment_group.id}"}
                  icon="arrow-down-s-line"
                  class="block"
                />
              </button>

              <div id={"accordion-collapse-body-#{deployment_group.id}"} class="hidden">
                <div class="p-5">
                  <.live_component
                    module={LivebookWeb.Hub.SecretListComponent}
                    id={"deployment-group-secrets-list-#{deployment_group.id}"}
                    hub={@hub}
                    secrets={deployment_group.secrets}
                    deployment_group={@deployment_group}
                    add_path={
                      ~p"/hub/#{@hub.id}/deployment-groups/edit/#{deployment_group.id}/secrets/new"
                    }
                    edit_path={"hub/#{@hub.id}/deployment-groups/edit/#{deployment_group.id}/secrets/edit"}
                    return_to={~p"/hub/#{@hub.id}/deployment-groups/edit/#{deployment_group.id}"}
                  />
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
      <div>
        <.button patch={~p"/hub/#{@hub.id}/deployment-groups/new"} id="add-deployment-group">
          Add deployment group
        </.button>
      </div>
    </div>
    """
  end
end

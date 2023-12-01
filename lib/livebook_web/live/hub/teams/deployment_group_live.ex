defmodule LivebookWeb.Hub.Teams.DeploymentGroupLive do
  use LivebookWeb, :live_view

  alias LivebookWeb.LayoutHelpers
  alias Livebook.Hubs
  alias Livebook.Teams
  alias Livebook.Hubs.Provider
  alias LivebookWeb.NotFoundError

  on_mount LivebookWeb.SidebarHook

  @impl true
  def handle_params(%{"id" => id} = params, _url, socket) do
    hub = Hubs.fetch_hub!(id)
    deployment_group_id = params["deployment_group_id"]
    secret_name = params["secret_name"]
    deployment_groups = Teams.get_deployment_groups(hub)
    default? = default_hub?(hub)

    deployment_group =
      if socket.assigns.live_action != :new_deployment_group do
        Enum.find_value(deployment_groups, &(&1.id == deployment_group_id && &1)) ||
          raise(
            NotFoundError,
            "could not find deployment group matching #{inspect(deployment_group_id)}"
          )
      end

    secrets =
      if socket.assigns.live_action != :new_deployment_group,
        do: deployment_group.secrets,
        else: []

    secret_value =
      if socket.assigns.live_action == :edit_secret do
        Enum.find_value(secrets, &(&1.name == secret_name and &1.value)) ||
          raise(NotFoundError, "could not find secret matching #{inspect(secret_name)}")
      end

    {:noreply,
     socket
     |> assign(
       hub: hub,
       deployment_groups: deployment_groups,
       deployment_group_id: deployment_group_id,
       deployment_group: deployment_group,
       hub_metadata: Provider.to_metadata(hub),
       secret_name: secret_name,
       secret_value: secret_value,
       default?: default?,
       secrets: secrets
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <LayoutHelpers.layout
      current_page={~p"/hub/#{@hub.id}"}
      current_user={@current_user}
      saved_hubs={@saved_hubs}
    >
      <div>
        <LayoutHelpers.topbar
          :if={not @hub_metadata.connected? && Provider.connection_error(@hub)}
          variant={:warning}
        >
          <%= Provider.connection_error(@hub) %>
        </LayoutHelpers.topbar>

        <div class="p-4 md:px-12 md:py-7 max-w-screen-md mx-auto">
          <div id={"#{@hub.id}-component"}>
            <div class="mb-8 flex flex-col space-y-10">
              <div class="flex flex-col space-y-2">
                <LayoutHelpers.title>
                  <div class="flex gap-2 items-center">
                    <div class="flex justify-center">
                      <span class="relative">
                        <%= @hub.hub_emoji %>

                        <div class={[
                          "absolute w-[10px] h-[10px] border-white border-2 rounded-full right-0 bottom-1",
                          if(@hub_metadata.connected?, do: "bg-green-400", else: "bg-red-400")
                        ]} />
                      </span>
                    </div>
                    <%= @hub.hub_name %>
                    <span class="bg-green-100 text-green-800 text-xs px-2.5 py-0.5 rounded cursor-default">
                      Livebook Teams
                    </span>
                    <%= if @default? do %>
                      <span class="bg-blue-100 text-blue-800 text-xs px-2.5 py-0.5 rounded cursor-default">
                        Default
                      </span>
                    <% end %>
                  </div>
                </LayoutHelpers.title>
                <p class="text-sm flex flex-row space-x-6 text-gray-700">
                  <.link patch={~p"/hub/#{@hub.id}"} class="hover:text-blue-600 cursor-pointer">
                    <.remix_icon icon="arrow-left-line" /> Back to Hub
                  </.link>
                </p>
              </div>

              <div class="flex flex-col space-y-4">
                <.live_component
                  module={LivebookWeb.Hub.Teams.DeploymentGroupFormComponent}
                  id="deployment-groups"
                  hub={@hub}
                  deployment_group_id={@deployment_group_id}
                  deployment_group={@deployment_group}
                  return_to={~p"/hub/#{@hub.id}"}
                />
              </div>

              <%= if @deployment_group_id do %>
                <div class="flex flex-col space-y-4">
                  <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
                    Secrets
                  </h2>

                  <p class="text-gray-700">
                    Deployment group secrets overrides Hub secrets
                  </p>

                  <.live_component
                    module={LivebookWeb.Hub.SecretListComponent}
                    id="hub-secrets-list"
                    hub={@hub}
                    secrets={@secrets}
                    secrets_origin={:deployment_group}
                    deployment_group={@deployment_group}
                    add_path={
                      ~p"/hub/#{@hub.id}/deployment-groups/edit/#{@deployment_group.id}/secrets/new"
                    }
                    edit_path={"hub/#{@hub.id}/deployment-groups/edit/#{@deployment_group.id}/secrets/edit"}
                    return_to={~p"/hub/#{@hub.id}/deployment-groups/edit/#{@deployment_group.id}"}
                  />
                </div>
              <% end %>
            </div>
          </div>
        </div>
      </div>
      <.modal
        :if={@live_action in [:new_secret, :edit_secret]}
        id="secrets-modal"
        show
        width={:medium}
        patch={~p"/hub/#{@hub.id}/deployment-groups/edit/#{@deployment_group.id}"}
      >
        <.live_component
          module={LivebookWeb.Hub.SecretFormComponent}
          id="secrets"
          hub={@hub}
          deployment_group_id={@deployment_group.id}
          secret_name={@secret_name}
          secret_value={@secret_value}
          return_to={~p"/hub/#{@hub.id}/deployment-groups/edit/#{@deployment_group.id}"}
        />
      </.modal>
    </LayoutHelpers.layout>
    """
  end

  defp default_hub?(hub) do
    Hubs.get_default_hub().id == hub.id
  end
end

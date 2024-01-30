defmodule LivebookWeb.Hub.Teams.DeploymentGroupListComponent do
  use LivebookWeb, :live_component

  alias Livebook.Teams

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="flex flex-col space-y-4">
      <div class="flex flex-col space-y-4">
        <.no_entries :if={@deployment_groups == []}>
          No deployment groups in this Hub... yet!
        </.no_entries>
        <div :if={@deployment_groups != []}>
          <.table id="hub-deployment-groups-table" rows={@deployment_groups}>
            <:col :let={deployment_group} label="Name"><%= deployment_group.name %></:col>
            <:col :let={deployment_group} label="Mode">
              <%= deployment_group.mode %>
            </:col>
            <:action :let={deployment_group}>
              <span class="tooltip left" data-tooltip="Edit">
                <.link
                  id={"hub-deployment-group-#{deployment_group.id}-edit"}
                  patch={~p"/hub/#{@hub_id}/deployment-groups/edit/#{deployment_group.id}"}
                  type="button"
                  class="icon-button"
                >
                  <.remix_icon icon="edit-fill" class="text-lg" />
                </.link>
              </span>
            </:action>
            <:action :let={deployment_group}>
              <span class="tooltip left" data-tooltip="Delete">
                <button
                  id={"hub-deployment-group-#{deployment_group.id}-delete"}
                  type="button"
                  phx-click={
                    JS.push("delete_deployment_group",
                      value: %{id: deployment_group.id, name: deployment_group.name}
                    )
                  }
                  phx-target={@myself}
                  class="icon-button"
                >
                  <.remix_icon icon="delete-bin-6-line" class="text-lg" />
                </button>
              </span>
            </:action>
          </.table>
        </div>
      </div>
      <div class="flex">
        <.link
          patch={~p"/hub/#{@hub_id}/deployment-groups/new"}
          class="button-base button-blue"
          id="add-deployment-group"
        >
          Add deployment group
        </.link>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("delete_deployment_group", %{"id" => id, "name" => name}, socket) do
    on_confirm = fn socket ->
      hub = Livebook.Hubs.fetch_hub!(socket.assigns.hub.id)
      deployment_groups = Teams.get_deployment_groups(hub)
      deployment_group = Enum.find(deployment_groups, &(&1.id == id))

      case Teams.delete_deployment_group(hub, deployment_group) do
        :ok ->
          socket
          |> put_flash(:success, "Deployment group #{deployment_group.name} deleted successfully")
          |> push_patch(to: ~p"/hub/#{hub.id}")

        {:transport_error, reason} ->
          put_flash(socket, :error, reason)
      end
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Delete hub deployment group",
       description: "Are you sure you want to delete #{name}?",
       confirm_text: "delete",
       confirm_icon: "delete-bin-6-line"
     )}
  end
end

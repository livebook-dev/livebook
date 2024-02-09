defmodule LivebookWeb.Hub.Teams.DeploymentGroupListComponent do
  use LivebookWeb, :live_component

  alias Livebook.Teams

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="flex flex-col space-y-4">
      <div class="flex flex-col space-y-4">
        <.no_entries :if={@deployment_groups == []}>
          No deployment groups in this Hub yet.
        </.no_entries>
        <div
          :for={deployment_group <- @deployment_groups}
          class="flex items-center justify-between border border-gray-200 rounded-lg p-4"
        >
          <div class="flex items-center space-x-12">
            <.labeled_text label="Name"><%= deployment_group.name %></.labeled_text>
            <.labeled_text label="Mode"><%= deployment_group.mode %></.labeled_text>
          </div>
          <div class="flex items-center space-x-2">
            <.menu id={"hub-deployment-group-#{deployment_group.id}-menu"}>
              <:toggle>
                <.icon_button aria-label="open deployment group menu" type="button">
                  <.remix_icon icon="more-2-fill" />
                </.icon_button>
              </:toggle>
              <.menu_item>
                <.link
                  id={"hub-deployment-group-#{deployment_group.id}-edit"}
                  patch={~p"/hub/#{@hub_id}/deployment-groups/edit/#{deployment_group.id}"}
                  type="button"
                  role="menuitem"
                >
                  <.remix_icon icon="file-edit-line" />
                  <span>Edit</span>
                </.link>
              </.menu_item>
              <.menu_item variant={:danger}>
                <button
                  id={"hub-deployment-group-#{deployment_group.id}-delete"}
                  type="button"
                  role="menuitem"
                  class="text-red-600"
                  phx-click={
                    JS.push("delete_deployment_group",
                      value: %{id: deployment_group.id, name: deployment_group.name}
                    )
                  }
                  phx-target={@myself}
                >
                  <.remix_icon icon="delete-bin-line" />
                  <span>Delete</span>
                </button>
              </.menu_item>
            </.menu>
          </div>
        </div>
      </div>
      <div class="flex">
        <.button patch={~p"/hub/#{@hub_id}/deployment-groups/new"} id="add-deployment-group">
          Add deployment group
        </.button>
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

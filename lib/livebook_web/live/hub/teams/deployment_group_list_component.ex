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
end

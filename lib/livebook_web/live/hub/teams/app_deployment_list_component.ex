defmodule LivebookWeb.Hub.Teams.AppDeploymentListComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="flex flex-col space-y-4">
      <div class="flex flex-col space-y-4">
        <.no_entries :if={@app_deployments == []}>
          No deployed apps in this deployment group yet.
        </.no_entries>
        <div :if={@app_deployments != []}>
          <.table id="hub-deployed-apps-table" rows={@app_deployments}>
            <:col :let={app_deployment} label="ID"><%= app_deployment.id %></:col>
            <:col :let={app_deployment} label="Slug"><%= app_deployment.slug %></:col>
            <:col :let={app_deployment} label="Deployed By"><%= app_deployment.deployed_by %></:col>
            <:col :let={app_deployment} label="Deployed At"><%= app_deployment.deployed_at %></:col>
          </.table>
        </div>
      </div>
    </div>
    """
  end
end

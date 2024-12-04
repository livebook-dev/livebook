defmodule LivebookWeb.SettingsLive.EnvVarsComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    assigns =
      assigns
      |> assign_new(:target, fn -> nil end)
      |> assign_new(:edit_label, fn -> "Edit" end)

    ~H"""
    <div id={@id} class="flex flex-col space-y-4">
      <div class="flex flex-col space-y-4">
        <.no_entries :if={@env_vars == []}>
          No environment variables here... yet!
        </.no_entries>
        <div :if={@env_vars != []}>
          <.table id="env-var-table" rows={@env_vars}>
            <:col :let={env_var} label="Name">{env_var.name}</:col>
            <:action :let={env_var}>
              <span class="tooltip left" data-tooltip="Edit">
                <.icon_button
                  id={"env-var-#{env_var.name}-edit"}
                  type="button"
                  phx-click={
                    JS.push("edit_env_var", value: %{env_var: env_var.name}, target: @target)
                  }
                >
                  <.remix_icon icon="edit-fill" />
                </.icon_button>
              </span>
            </:action>
            <:action :let={env_var}>
              <span class="tooltip left" data-tooltip="Delete">
                <.icon_button
                  id={"env-var-#{env_var.name}-delete"}
                  type="button"
                  phx-click={
                    JS.push("delete_env_var", value: %{env_var: env_var.name}, target: @target)
                  }
                >
                  <.remix_icon icon="delete-bin-6-line" />
                </.icon_button>
              </span>
            </:action>
          </.table>
        </div>
      </div>
    </div>
    """
  end
end

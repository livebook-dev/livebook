defmodule LivebookWeb.SettingsLive.EnvironmentVariablesComponent do
  use LivebookWeb, :live_component

  alias Livebook.Settings

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="flex flex-col space-y-4">
      <div class="flex flex-col space-y-4">
        <%= for env_var <- @env_vars do %>
          <div class="flex items-center justify-between border border-gray-200 rounded-lg p-4">
            <.env_var_info socket={@socket} env_var={env_var} myself={@myself} />
          </div>
        <% end %>
      </div>
      <div class="flex">
        <%= live_patch("Add environment variable",
          to: Routes.settings_path(@socket, :env_var),
          class: "button-base button-blue"
        ) %>
      </div>
    </div>
    """
  end

  defp env_var_info(assigns) do
    ~H"""
    <div class="grid grid-cols-1 md:grid-cols-2 w-full">
      <div class="place-content-start">
        <.labeled_text label="Key">
          <%= @env_var.key %>
        </.labeled_text>
      </div>

      <div class="flex items-center place-content-end">
        <.menu id={"env-var-#{@env_var.id}-menu"}>
          <:toggle>
            <button class="icon-button" aria-label="open session menu" type="button">
              <.remix_icon icon="more-2-fill" class="text-xl" />
            </button>
          </:toggle>
          <:content>
            <button
              id={"env-var-#{@env_var.id}-edit"}
              type="button"
              phx-click={JS.push("edit_env_var", value: %{env_var: @env_var.id})}
              phx-target={@myself}
              role="menuitem"
              class="menu-item text-gray-600"
            >
              <.remix_icon icon="file-edit-line" />
              <span class="font-medium">Edit</span>
            </button>
            <button
              id={"env-var-#{@env_var.id}-delete"}
              type="button"
              phx-click={
                with_confirm(
                  JS.push("delete_env_var", value: %{env_var: @env_var.id}),
                  title: "Delete #{@env_var.key}",
                  description: "Are you sure you want to delete environment variable?",
                  confirm_text: "Delete",
                  confirm_icon: "delete-bin-6-line"
                )
              }
              phx-target={@myself}
              role="menuitem"
              class="menu-item text-red-600"
            >
              <.remix_icon icon="delete-bin-line" />
              <span class="font-medium">Delete</span>
            </button>
          </:content>
        </.menu>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("edit_env_var", %{"env_var" => id}, socket) do
    env_var = Settings.fetch_env_var!(id)
    send(self(), {:edit_env_var, env_var})

    {:noreply, socket}
  end

  def handle_event("delete_env_var", %{"env_var" => id}, socket) do
    Settings.delete_env_var(id)
    send(self(), {:env_vars_updated, Settings.fetch_env_vars()})

    {:noreply, socket}
  end
end

defmodule LivebookWeb.EnvVarsComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    assigns = assign_new(assigns, :target, fn -> nil end)

    ~H"""
    <div id={@id} class="flex flex-col space-y-4">
      <div class="flex flex-col space-y-4">
        <%= for env_var <- @env_vars do %>
          <div class="flex items-center justify-between border border-gray-200 rounded-lg p-4">
            <.env_var_info socket={@socket} env_var={env_var} target={@target} />
          </div>
        <% end %>
      </div>
      <div class="flex">
        <%= live_patch("Add environment variable",
          to: @add_env_var_path,
          id: "add-env-var",
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
        <.labeled_text label="Name">
          <%= @env_var.name %>
        </.labeled_text>
      </div>

      <div class="flex items-center place-content-end">
        <.menu id={"env-var-#{@env_var.name}-menu"}>
          <:toggle>
            <button class="icon-button" aria-label="open session menu" type="button">
              <.remix_icon icon="more-2-fill" class="text-xl" />
            </button>
          </:toggle>
          <:content>
            <button
              id={"env-var-#{@env_var.name}-edit"}
              type="button"
              phx-click={JS.push("edit_env_var", value: %{env_var: @env_var.name})}
              phx-target={@target}
              role="menuitem"
              class="menu-item text-gray-600"
            >
              <.remix_icon icon="file-edit-line" />
              <span class="font-medium">Edit</span>
            </button>
            <button
              id={"env-var-#{@env_var.name}-delete"}
              type="button"
              phx-click={
                with_confirm(
                  JS.push("delete_env_var", value: %{env_var: @env_var.name}),
                  title: "Delete #{@env_var.name}",
                  description: "Are you sure you want to delete environment variable?",
                  confirm_text: "Delete",
                  confirm_icon: "delete-bin-6-line"
                )
              }
              phx-target={@target}
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
end

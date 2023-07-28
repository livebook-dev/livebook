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
        <div
          :for={env_var <- @env_vars}
          class="flex items-center justify-between border border-gray-200 rounded-lg p-4"
        >
          <.env_var_info env_var={env_var} edit_label={@edit_label} target={@target} />
        </div>
      </div>
      <div class="flex">
        <.link patch={@add_env_var_path} class="button-base button-blue" id="add-env-var">
          Add environment variable
        </.link>
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
            <button class="icon-button" aria-label="open environment variable menu" type="button">
              <.remix_icon icon="more-2-fill" class="text-xl" />
            </button>
          </:toggle>
          <.menu_item>
            <button
              id={"env-var-#{@env_var.name}-edit"}
              type="button"
              phx-click={JS.push("edit_env_var", value: %{env_var: @env_var.name})}
              phx-target={@target}
              role="menuitem"
            >
              <.remix_icon icon="file-edit-line" />
              <span><%= @edit_label %></span>
            </button>
          </.menu_item>
          <.menu_item variant={:danger}>
            <button
              id={"env-var-#{@env_var.name}-delete"}
              type="button"
              phx-click={JS.push("delete_env_var", value: %{env_var: @env_var.name})}
              phx-target={@target}
              role="menuitem"
            >
              <.remix_icon icon="delete-bin-line" />
              <span>Delete</span>
            </button>
          </.menu_item>
        </.menu>
      </div>
    </div>
    """
  end
end

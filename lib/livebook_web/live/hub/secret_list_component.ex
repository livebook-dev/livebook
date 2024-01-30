defmodule LivebookWeb.Hub.SecretListComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs
  alias Livebook.Secrets
  alias Livebook.Secrets.Secret

  @impl true
  def update(assigns, socket) do
    {:ok, assign(socket, assigns)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="flex flex-col space-y-4">
      <div class="flex flex-col space-y-4">
        <.no_entries :if={@secrets == []}>
          No secrets here... yet!
        </.no_entries>
        <div :if={@secrets != []}>
          <.table id="hub-secrets-table" rows={@secrets}>
            <:col :let={secret} label="Name"><%= secret.name %></:col>
            <:action :let={secret}>
              <span class="tooltip left" data-tooltip="Edit">
                <.link
                  id={"hub-secret-#{secret.name}-edit"}
                  patch={"/#{@edit_path}/#{secret.name}"}
                  type="button"
                  role="menuitem"
                  class="icon-button"
                >
                  <.remix_icon icon="edit-fill" class="text-lg" />
                </.link>
              </span>
            </:action>
            <:action :let={secret}>
              <span class="tooltip left" data-tooltip="Delete">
                <button
                  id={"hub-secret-#{secret.name}-delete"}
                  type="button"
                  phx-click={
                    JS.push("delete_hub_secret",
                      value: %{
                        name: secret.name,
                        value: secret.value,
                        hub_id: secret.hub_id,
                        deployment_group_id: secret.deployment_group_id,
                        return_to: @return_to
                      },
                      target: @myself
                    )
                  }
                  role="menuitem"
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
        <.link patch={@add_path} class="button-base button-blue" id="add-secret">
          Add secret
        </.link>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("delete_hub_secret", attrs, socket) do
    on_confirm = fn socket ->
      {:ok, secret} = Secrets.update_secret(%Secret{}, attrs)
      hub = Hubs.fetch_hub!(secret.hub_id)

      case Hubs.delete_secret(hub, secret) do
        :ok ->
          socket
          |> put_flash(:success, "Secret #{secret.name} deleted successfully")
          |> push_patch(to: attrs["return_to"])

        {:transport_error, reason} ->
          put_flash(socket, :error, reason)
      end
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Delete hub secret - #{attrs["name"]}",
       description: "Are you sure you want to delete this hub secret?",
       confirm_text: "Delete",
       confirm_icon: "delete-bin-6-line"
     )}
  end
end

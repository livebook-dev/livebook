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
          No secrets in this Hub yet.
        </.no_entries>
        <div
          :for={secret <- @secrets}
          class="flex items-center justify-between border border-gray-200 rounded-lg p-4"
        >
          <div class="grid grid-cols-1 md:grid-cols-2 w-full">
            <div class="place-content-start">
              <.labeled_text label="Name">
                <%= secret.name %>
              </.labeled_text>
            </div>

            <div class="flex items-center place-content-end">
              <.menu id={"hub-secret-#{secret.name}-menu"}>
                <:toggle>
                  <button
                    class="icon-button"
                    aria-label="open environment variable menu"
                    type="button"
                  >
                    <.remix_icon icon="more-2-fill" class="text-xl" />
                  </button>
                </:toggle>
                <.menu_item>
                  <.link
                    id={"hub-secret-#{secret.name}-edit"}
                    patch={~p"/hub/#{secret.hub_id}/secrets/edit/#{secret.name}"}
                    type="button"
                    role="menuitem"
                  >
                    <.remix_icon icon="file-edit-line" />
                    <span>Edit</span>
                  </.link>
                </.menu_item>
                <.menu_item variant={:danger}>
                  <button
                    id={"hub-secret-#{secret.name}-delete"}
                    type="button"
                    phx-click={
                      JS.push("delete_hub_secret",
                        value: %{
                          name: secret.name,
                          value: secret.value,
                          hub_id: secret.hub_id
                        }
                      )
                    }
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
        </div>
      </div>
      <div class="flex">
        <.link patch={~p"/hub/#{@hub.id}/secrets/new"} class="button-base button-blue" id="add-secret">
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
      hub = Livebook.Hubs.fetch_hub!(secret.hub_id)

      case Hubs.delete_secret(hub, secret) do
        :ok -> socket
        {:transport_error, reason} -> put_flash(socket, :error, reason)
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

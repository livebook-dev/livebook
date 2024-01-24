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
          <table class="min-w-full divide-y divide-gray-300">
            <thead>
              <tr>
                <th
                  scope="col"
                  class="py-3.5 pl-4 pr-3 text-left text-sm font-semibold text-gray-900 sm:pl-6"
                >
                  Name
                </th>
                <th
                  scope="col"
                  class="py-3.5 pl-3 pr-4 text-right text-sm font-semibold text-gray-900 sm:pr-6"
                >
                  Actions
                </th>
              </tr>
            </thead>
            <tbody class="divide-y divide-gray-200 bg-white">
              <tr :for={secret <- @secrets}>
                <td class="whitespace-nowrap py-4 pl-4 pr-3 text-sm font-medium text-gray-900 sm:pl-6">
                  <%= secret.name %>
                </td>
                <td class="relative whitespace-nowrap py-4 pl-3 pr-4 flex justify-end items-center gap-4 text-sm font-medium sm:pr-6">
                  <span class="tooltip left" data-tooltip="Edit">
                    <.link
                      id={"hub-secret-#{secret.name}-edit"}
                      patch={"/#{@edit_path}/#{secret.name}"}
                      type="button"
                      role="menuitem"
                      class="text-gray-600 hover:text-blue-600"
                    >
                      <.remix_icon icon="edit-fill" class="text-lg" />
                    </.link>
                  </span>
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
                          }
                        )
                      }
                      phx-target={@myself}
                      role="menuitem"
                      class="text-gray-600 hover:text-red-700"
                    >
                      <.remix_icon icon="delete-bin-6-line" class="text-lg" />
                    </button>
                  </span>
                </td>
              </tr>
            </tbody>
          </table>
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

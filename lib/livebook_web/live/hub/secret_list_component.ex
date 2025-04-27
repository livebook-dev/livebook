defmodule LivebookWeb.Hub.SecretListComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs
  alias Livebook.Secrets
  alias Livebook.Secrets.Secret

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-4">
      <div class="flex flex-col space-y-4">
        <.no_entries :if={@secrets == []}>
          No secrets here... yet!
        </.no_entries>
        <div :if={@secrets != []}>
          <.table rows={@secrets} id={@id}>
            <:col :let={secret} label="Name">{secret.name}</:col>
            <:action :let={secret}>
              <span class="tooltip left" data-tooltip="Edit">
                <.icon_button
                  aria-label={"edit #{secret.name}"}
                  patch={"/#{@edit_path}/#{secret.name}"}
                  type="button"
                  role="menuitem"
                >
                  <.remix_icon icon="edit-fill" />
                </.icon_button>
              </span>
            </:action>
            <:action :let={secret}>
              <span class="tooltip left" data-tooltip="Delete">
                <.icon_button
                  aria-label={"delete #{secret.name}"}
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

    assigns = %{name: attrs["name"]}

    description = ~H"""
    Are you sure you want to delete this secret - <span class="font-semibold">{@name}</span>?
    """

    {:noreply,
     confirm(socket, on_confirm,
       title: "Delete secret",
       description: description,
       confirm_text: "Delete",
       confirm_icon: "delete-bin-6-line"
     )}
  end
end

defmodule LivebookWeb.Hub.Teams.AgentKeyListComponent do
  use LivebookWeb, :live_component

  alias Livebook.Teams

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="flex flex-col space-y-4">
      <div class="flex flex-col space-y-4">
        <.no_entries :if={@agent_keys == []}>
          No agent keys in this deployment group yet.
        </.no_entries>
        <div :if={@agent_keys != []}>
          <.table id="hub-agent-keys-table" rows={@agent_keys}>
            <:col :let={agent_key} label="ID"><%= agent_key.id %></:col>
            <:col :let={agent_key} label="Key">
              <div class="flex flex-nowrap gap-2">
                <div class="grow">
                  <.password_field
                    id={"agent-key-#{agent_key.id}"}
                    name="agent_key"
                    value={agent_key.key}
                    readonly
                  />
                </div>

                <span
                  data-tooltip="Copied to clipboard"
                  aria-label="copy to clipboard"
                  phx-click={
                    JS.dispatch("lb:clipcopy", to: "#agent-key-#{agent_key.id}")
                    |> JS.transition("tooltip top", time: 2000)
                  }
                >
                  <.button color="gray" small type="button">
                    <.remix_icon icon="clipboard-line" class="text-xl leading-none py-1" />
                  </.button>
                </span>
              </div>
            </:col>
            <:action :let={agent_key}>
              <span class="tooltip left" data-tooltip="Delete">
                <.icon_button
                  id={"hub-agent-key-#{agent_key.id}-delete"}
                  type="button"
                  phx-click={
                    JS.push("delete_agent_key",
                      value: %{id: agent_key.id},
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
  def handle_event("delete_agent_key", %{"id" => id}, socket) do
    on_confirm = fn socket ->
      hub = Livebook.Hubs.fetch_hub!(socket.assigns.hub.id)
      agent_keys = Teams.get_agent_keys(hub, socket.assigns.deployment_group_id)
      agent_key = Enum.find(agent_keys, &(&1.id == id))

      case Teams.delete_agent_key(hub, agent_key) do
        :ok ->
          socket
          |> put_flash(:success, "Agent key deleted successfully")
          |> push_patch(
            to: ~p"/hub/#{hub.id}/deployment-groups/edit/#{agent_key.deployment_group_id}"
          )

        {:transport_error, reason} ->
          put_flash(socket, :error, reason)
      end
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Delete hub agent key",
       description: "Are you sure you want to delete?",
       confirm_text: "Delete",
       confirm_icon: "delete-bin-6-line"
     )}
  end
end

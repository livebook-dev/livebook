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
        <div
          :for={agent_key <- @agent_keys}
          class="flex items-center justify-between border border-gray-200 rounded-lg p-4"
        >
          <div class="flex items-center space-x-12">
            <.labeled_text label="ID">
              <div class="flex h-[40px] items-center text-center">
                <%= agent_key.id %>
              </div>
            </.labeled_text>

            <.labeled_text label="Key">
              <div id="agent-key-toggle" class="relative lg:w-[480px] w-full">
                <input
                  type="password"
                  id="agent-key"
                  readonly
                  value={agent_key.key}
                  class="input font-mono w-full border-neutral-200 bg-neutral-100 py-2 border-2 pr-8"
                />

                <div class="flex items-center absolute inset-y-0 right-1">
                  <button
                    class="icon-button"
                    data-tooltip="Copied to clipboard"
                    type="button"
                    aria-label="copy to clipboard"
                    phx-click={
                      JS.dispatch("lb:clipcopy", to: "#agent-key")
                      |> JS.add_class("", transition: {"tooltip top", "", ""}, time: 2000)
                    }
                  >
                    <.remix_icon icon="clipboard-line" class="text-xl" />
                  </button>

                  <button
                    class="icon-button"
                    data-show
                    type="button"
                    aria-label="show password"
                    phx-click={
                      JS.remove_attribute("type", to: "#agent-key-toggle input")
                      |> JS.set_attribute({"type", "text"}, to: "#agent-key-toggle input")
                      |> toggle_class("hidden", to: "#agent-key-toggle [data-show]")
                      |> toggle_class("hidden", to: "#agent-key-toggle [data-hide]")
                    }
                  >
                    <.remix_icon icon="eye-line" class="text-xl" />
                  </button>
                  <button
                    class="icon-button hidden"
                    data-hide
                    type="button"
                    aria-label="hide password"
                    phx-click={
                      JS.remove_attribute("type", to: "#agent-key-toggle input")
                      |> JS.set_attribute({"type", "password"}, to: "#agent-key-toggle input")
                      |> toggle_class("hidden", to: "#agent-key-toggle [data-show]")
                      |> toggle_class("hidden", to: "#agent-key-toggle [data-hide]")
                    }
                  >
                    <.remix_icon icon="eye-off-line" class="text-xl" />
                  </button>
                </div>
              </div>
            </.labeled_text>
          </div>
          <div class="flex items-center space-x-2">
            <.menu id={"hub-agent-key-#{agent_key.id}-menu"}>
              <:toggle>
                <button class="icon-button" aria-label="open deployment group menu" type="button">
                  <.remix_icon icon="more-2-fill" class="text-xl" />
                </button>
              </:toggle>
              <.menu_item variant={:danger}>
                <button
                  id={"hub-agent-key-#{agent_key.id}-delete"}
                  type="button"
                  role="menuitem"
                  class="text-red-600"
                  phx-click={JS.push("delete_agent_key", value: %{id: agent_key.id})}
                  phx-target={@myself}
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

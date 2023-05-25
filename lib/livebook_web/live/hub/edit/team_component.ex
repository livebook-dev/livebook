defmodule LivebookWeb.Hub.Edit.TeamComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs.Team
  alias Livebook.Teams
  alias LivebookWeb.LayoutHelpers

  @impl true
  def update(assigns, socket) do
    changeset = Team.change_hub(assigns.hub)

    {:ok,
     socket
     |> assign(assigns)
     |> assign_form(changeset)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={"#{@id}-component"}>
      <.modal
        :if={@show_key}
        id="show-key-modal"
        width={:medium}
        show={true}
        patch={~p"/hub/#{@hub.id}"}
      >
        <div class="p-6 flex flex-col space-y-5">
          <h3 class="text-2xl font-semibold text-gray-800">
            Teams Key
          </h3>
          <div class="justify-center">
            This is your <strong>Teams Key</strong> and we strongly recommend
            you to store it somewhere safe:
          </div>
          <div class=" w-full">
            <div id="teams-key-toggle" class="relative flex">
              <input
                type="password"
                id="teams-key"
                readonly
                value={Phoenix.HTML.Form.normalize_value("text", @hub.teams_key)}
                class="input font-mono w-full border-neutral-200 bg-neutral-100 py-2 border-2 pr-8"
              />

              <div class="flex items-center absolute inset-y-0 right-1">
                <button
                  class="icon-button"
                  data-copy
                  data-tooltip="Copied to clipboard"
                  type="button"
                  aria-label="copy to clipboard"
                  phx-click={
                    JS.dispatch("phx:copy", to: "#teams-key")
                    |> JS.add_class(
                      "tooltip top",
                      to: "#teams-key-toggle [data-copy]",
                      transition: {"ease-out duration-200", "opacity-0", "opacity-100"}
                    )
                    |> JS.remove_class(
                      "tooltip top",
                      to: "#teams-key-toggle [data-copy]",
                      transition: {"ease-out duration-200", "opacity-0", "opacity-100"},
                      time: 2000
                    )
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
                    JS.remove_attribute("type", to: "#teams-key-toggle input")
                    |> JS.set_attribute({"type", "text"}, to: "#teams-key-toggle input")
                    |> JS.add_class("hidden", to: "#teams-key-toggle [data-show]")
                    |> JS.remove_class("hidden", to: "#teams-key-toggle [data-hide]")
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
                    JS.remove_attribute("type", to: "#teams-key-toggle input")
                    |> JS.set_attribute({"type", "password"}, to: "#teams-key-toggle input")
                    |> JS.remove_class("hidden", to: "#teams-key-toggle [data-show]")
                    |> JS.add_class("hidden", to: "#teams-key-toggle [data-hide]")
                  }
                >
                  <.remix_icon icon="eye-off-line" class="text-xl" />
                </button>
              </div>
            </div>
          </div>
          <div class="justify-center">
            With this key, you can share within your team members to join your organization!
          </div>
        </div>
      </.modal>

      <div class="space-y-8">
        <div class="flex relative">
          <LayoutHelpers.title text={"#{@hub.hub_emoji} #{@hub.hub_name}"} />

          <button
            phx-click={JS.push("delete_hub", value: %{id: @hub.id})}
            class="absolute right-0 button-base button-red"
          >
            Delete hub
          </button>
        </div>

        <div class="flex flex-col space-y-10">
          <div class="flex flex-col space-y-2">
            <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
              General
            </h2>

            <.form
              :let={f}
              id={@id}
              class="flex flex-col mt-4 space-y-4"
              for={@form}
              phx-submit="save"
              phx-change="validate"
              phx-target={@myself}
            >
              <div class="grid grid-cols-1 md:grid-cols-1 gap-3">
                <.emoji_field field={f[:hub_emoji]} label="Emoji" />
              </div>

              <button class="button-base button-blue" type="submit" phx-disable-with="Updating...">
                Update Hub
              </button>
            </.form>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("save", %{"team" => params}, socket) do
    case Teams.update_hub(socket.assigns.hub, params) do
      {:ok, hub} ->
        {:noreply,
         socket
         |> put_flash(:success, "Hub updated successfully")
         |> push_navigate(to: ~p"/hub/#{hub.id}")}

      {:error, changeset} ->
        {:noreply, assign_form(socket, changeset)}
    end
  end

  def handle_event("validate", %{"team" => attrs}, socket) do
    changeset =
      socket.assigns.hub
      |> Team.change_hub(attrs)
      |> Map.replace!(:action, :validate)

    {:noreply, assign_form(socket, changeset)}
  end

  defp assign_form(socket, %Ecto.Changeset{} = changeset) do
    assign(socket, form: to_form(changeset))
  end
end

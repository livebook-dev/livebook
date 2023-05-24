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
    <div id={"#{@id}-component"} class="space-y-8">
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

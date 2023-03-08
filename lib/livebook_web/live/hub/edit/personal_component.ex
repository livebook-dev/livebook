defmodule LivebookWeb.Hub.Edit.PersonalComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs.Personal
  alias LivebookWeb.LayoutHelpers

  @impl true
  def update(assigns, socket) do
    changeset = Personal.change_hub(assigns.hub)

    {:ok,
     socket
     |> assign(assigns)
     |> assign(changeset: changeset)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={"#{@id}-component"} class="space-y-8">
      <div class="space-y-4">
        <LayoutHelpers.title text={"#{@hub.hub_emoji} #{@hub.hub_name}"} />

        <p class="text-gray-700">
          Your personal hub. Only you can see and access the data in it.
        </p>
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
            for={@changeset}
            phx-submit="save"
            phx-change="validate"
            phx-target={@myself}
          >
            <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
              <.text_field field={f[:hub_name]} label="Name" />
              <.emoji_field field={f[:hub_emoji]} label="Emoji" />
            </div>
            <div>
              <button
                class="button-base button-blue"
                type="submit"
                phx-disable-with="Updating..."
                disable={not @changeset.valid?}
              >
                Update Hub
              </button>
            </div>
          </.form>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("save", %{"personal" => params}, socket) do
    case Personal.update_hub(socket.assigns.hub, params) do
      {:ok, hub} ->
        {:noreply,
         socket
         |> put_flash(:success, "Hub updated successfully")
         |> push_navigate(to: ~p"/hub/#{hub.id}")}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  def handle_event("validate", %{"personal" => attrs}, socket) do
    {:noreply, assign(socket, changeset: Personal.validate_hub(socket.assigns.hub, attrs))}
  end
end

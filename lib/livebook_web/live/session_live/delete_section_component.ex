defmodule LivebookWeb.SessionLive.DeleteSectionComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 pb-4 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Delete section
      </h3>
      <p class="text-gray-700">
        Are you sure you want to delete this section -
        <span class="font-semibold">“<%= @section.name %>”</span>?
      </p>
      <form phx-submit="delete" phx-target={@myself}>
        <h3 class="mb-1 text-lg font-semibold text-gray-800">
          Options
        </h3>
        <%# If there is no previous section, all cells need to be deleted %>
        <.switch_checkbox
          name="delete_cells"
          label="Delete all cells in this section"
          checked={@is_first}
          disabled={@is_first} />
        <div class="mt-8 flex justify-end space-x-2">
          <button type="submit" class="button-base button-red">
            <.remix_icon icon="delete-bin-6-line" class="align-middle mr-1" />
            Delete
          </button>
          <%= live_patch "Cancel", to: @return_to, class: "button-base button-outlined-gray" %>
        </div>
      </form>
    </div>
    """
  end

  @impl true
  def handle_event("delete", %{"delete_cells" => delete_cells}, socket) do
    delete_cells? = delete_cells == "true"

    Livebook.Session.delete_section(
      socket.assigns.session.pid,
      socket.assigns.section.id,
      delete_cells?
    )

    {:noreply, push_patch(socket, to: socket.assigns.return_to)}
  end
end

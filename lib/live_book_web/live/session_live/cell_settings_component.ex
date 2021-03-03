defmodule LiveBookWeb.SessionLive.CellSettingsComponent do
  use LiveBookWeb, :live_component

  alias LiveBook.Session

  @impl true
  def update(assigns, socket) do
    metadata = assigns.cell.metadata

    assigns =
      Map.merge(assigns, %{disable_formatting: Map.get(metadata, "disable_formatting", false)})

    {:ok, assign(socket, assigns)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="p-6 pb-4 max-w-4xl flex flex-col space-y-3">
      <h3 class="text-lg font-medium text-gray-900">
        Cell settings
      </h3>
      <form phx-submit="save" phx-target="<%= @myself %>">
        <div class="w-full flex-col space-y-3">
          <label class="flex space-x-3 items-center cursor-pointer">
            <%= tag :input, class: "checkbox-base", type: "checkbox", name: "disable_formatting", checked: @disable_formatting %>
            <span>Disable code formatting (when saving to file)</span>
          </label>
        </div>
        <div class="mt-6 flex justify-end space-x-2">
          <%= live_patch "Cancel", to: @return_to, class: "button-base button-sm" %>
          <button class="button-base button-primary button-sm" type="submit">
            Save
          </button>
        </div>
      </form>
    </div>
    """
  end

  @impl true
  def handle_event("save", params, socket) do
    metadata = update_metadata(socket.assigns.cell.metadata, params)
    Session.set_cell_metadata(socket.assigns.session_id, socket.assigns.cell.id, metadata)
    {:noreply, push_patch(socket, to: socket.assigns.return_to)}
  end

  defp update_metadata(metadata, form_data) do
    if Map.has_key?(form_data, "disable_formatting") do
      Map.put(metadata, "disable_formatting", true)
    else
      Map.delete(metadata, "disable_formatting")
    end
  end
end

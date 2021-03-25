defmodule LivebookWeb.SessionLive.CellSettingsComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

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
    <div class="p-6 pb-4 max-w-xl w-screen flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Cell settings
      </h3>
      <form phx-submit="save" phx-target="<%= @myself %>">
        <div class="w-full flex-col space-y-3">
          <div class="flex space-x-3 items-center justify-between">
            <span class="text-gray-700">Disable code formatting (when saving to file)</span>
            <label class="switch-button">
              <%= tag :input, class: "switch-button__checkbox", type: "checkbox", name: "disable_formatting", checked: @disable_formatting %>
              <div class="switch-button__bg"></div>
            </label>
          </div>
        </div>
        <div class="mt-8 flex justify-end space-x-2">
          <%= live_patch "Cancel", to: @return_to, class: "button button-outlined-gray" %>
          <button class="button button-blue" type="submit">
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

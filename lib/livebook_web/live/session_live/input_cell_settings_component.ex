defmodule LivebookWeb.SessionLive.InputCellSettingsComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  @impl true
  def update(assigns, socket) do
    cell = assigns.cell

    socket =
      socket
      |> assign(assigns)
      |> assign_new(:name, fn -> cell.name end)
      |> assign_new(:type, fn -> cell.type end)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="p-6 pb-4 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Cell settings
      </h3>
      <form phx-submit="save" phx-change="validate" phx-target="<%= @myself %>">
        <div class="flex space-x-8 items-center">
          <%= render_select("type", [number: "Number", password: "Password", text: "Text", url: "URL"], @type) %>
        </div>
        <div class="mt-4">
          <div class="input-label">Name</div>
          <input type="text" class="input" name="name" value="<%= @name %>" spellcheck="false" autocomplete="off" autofocus />
        </div>
        <div class="mt-8 flex justify-end space-x-2">
          <%= live_patch "Cancel", to: @return_to, class: "button button-outlined-gray" %>
          <%= content_tag :button, "Save",
                type: :submit,
                class: "button button-blue",
                disabled: @name == "" %>
        </div>
      </form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"name" => name, "type" => type}, socket) do
    type = String.to_existing_atom(type)
    {:noreply, assign(socket, name: name, type: type)}
  end

  def handle_event("save", %{"name" => name, "type" => type}, socket) do
    type = String.to_existing_atom(type)
    attrs = %{name: name, type: type}
    Session.set_cell_attributes(socket.assigns.session_id, socket.assigns.cell.id, attrs)
    {:noreply, push_patch(socket, to: socket.assigns.return_to)}
  end
end

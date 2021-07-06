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
      |> assign_new(:reactive, fn -> cell.reactive end)

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
        <div class="flex flex-col space-y-6">
          <div>
            <div class="input-label">Type</div>
            <%= render_select("type", input_types(), @type) %>
          </div>
          <div>
            <div class="input-label">Name</div>
            <input type="text" class="input" name="name" value="<%= @name %>" spellcheck="false" autocomplete="off" autofocus />
          </div>
          <div>
            <%= render_switch("reactive", @reactive, "Reactive (reevaluates dependent cells on change)") %>
          </div>
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
  def handle_event("validate", params, socket) do
    attrs = params_to_attrs(params)
    {:noreply, assign(socket, attrs)}
  end

  def handle_event("save", params, socket) do
    attrs = params_to_attrs(params)
    Session.set_cell_attributes(socket.assigns.session_id, socket.assigns.cell.id, attrs)
    {:noreply, push_patch(socket, to: socket.assigns.return_to)}
  end

  defp params_to_attrs(params) do
    name = params["name"]
    type = params["type"] |> String.to_existing_atom()
    reactive = Map.has_key?(params, "reactive")

    %{name: name, type: type, reactive: reactive}
  end

  defp input_types do
    [
      color: "Color",
      number: "Number",
      password: "Password",
      text: "Text",
      textarea: "Textarea",
      url: "URL",
      range: "Range"
    ]
  end
end

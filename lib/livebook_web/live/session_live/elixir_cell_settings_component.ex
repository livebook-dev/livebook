defmodule LivebookWeb.SessionLive.ElixirCellSettingsComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  @impl true
  def update(assigns, socket) do
    cell = assigns.cell

    socket =
      socket
      |> assign(assigns)
      |> assign_new(:disable_formatting, fn -> cell.disable_formatting end)
      |> assign_new(:evaluate_automatically?, fn -> cell.evaluate_automatically? end)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 pb-4 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Cell settings
      </h3>
      <form phx-submit="save" phx-target={@myself}>
        <div class="w-full flex-col space-y-6">
          <.switch_checkbox
            name="disable_formatting"
            label="Disable code formatting (when saving to file)"
            checked={@disable_formatting} />
        </div>
        <div class="w-full flex-col space-y-6 mt-4">
          <.switch_checkbox
            name="evaluate_automatically"
            label="Evaluate automatically"
            checked={@evaluate_automatically?} />
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
  def handle_event(
        "save",
        %{
          "disable_formatting" => disable_formatting,
          "evaluate_automatically" => evaluate_automatically
        },
        socket
      ) do
    disable_formatting = disable_formatting == "true"
    evaluate_automatically? = evaluate_automatically == "true"

    Session.set_cell_attributes(socket.assigns.session.pid, socket.assigns.cell.id, %{
      disable_formatting: disable_formatting,
      evaluate_automatically?: evaluate_automatically?
    })

    {:noreply, push_patch(socket, to: socket.assigns.return_to)}
  end
end

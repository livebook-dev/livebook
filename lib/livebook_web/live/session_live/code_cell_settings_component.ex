defmodule LivebookWeb.SessionLive.CodeCellSettingsComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  @impl true
  def update(assigns, socket) do
    cell = assigns.cell

    socket =
      socket
      |> assign(assigns)
      |> assign_new(:reevaluate_automatically, fn -> cell.reevaluate_automatically end)
      |> assign_new(:continue_on_error, fn -> cell.continue_on_error end)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Cell settings
      </h3>
      <form phx-submit="save" phx-target={@myself}>
        <div class="w-full flex-col space-y-6 mt-4">
          <.switch_field
            name="reevaluate_automatically"
            label="Reevaluate automatically"
            value={@reevaluate_automatically}
          />
        </div>
        <div class="w-full flex-col space-y-6 mt-4">
          <.switch_field
            name="continue_on_error"
            label="Continue on error"
            value={@continue_on_error}
          />
        </div>
        <div class="mt-8 flex justify-begin space-x-2">
          <.button type="submit">
            Save
          </.button>
          <.button color="gray" outlined patch={@return_to}>
            Cancel
          </.button>
        </div>
      </form>
    </div>
    """
  end

  @impl true
  def handle_event(
        "save",
        %{
          "reevaluate_automatically" => reevaluate_automatically,
          "continue_on_error" => continue_on_error
        },
        socket
      ) do
    reevaluate_automatically = reevaluate_automatically == "true"
    continue_on_error = continue_on_error == "true"

    Session.set_cell_attributes(socket.assigns.session.pid, socket.assigns.cell.id, %{
      reevaluate_automatically: reevaluate_automatically,
      continue_on_error: continue_on_error
    })

    {:noreply, push_patch(socket, to: socket.assigns.return_to)}
  end
end

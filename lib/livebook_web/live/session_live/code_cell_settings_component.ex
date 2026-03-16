defmodule LivebookWeb.SessionLive.CodeCellSettingsComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  @impl true
  def update(assigns, socket) do
    cell = assigns.cell

    socket =
      socket
      |> assign(assigns)
      |> assign_new(:output_size, fn -> cell.output_size end)
      |> assign_new(:output_size_options, fn ->
        for %{name: label, size: value} <- Livebook.Notebook.Cell.output_sizes() do
          {label, value}
        end
      end)
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
        <div class="flex flex-col w-full space-y-4 mt-4">
          <.switch_field
            name="reevaluate_automatically"
            label="Reevaluate automatically"
            value={@reevaluate_automatically}
          />
          <.switch_field
            name="continue_on_error"
            label="Continue on error"
            value={@continue_on_error}
          />
          <div class="flex flex w-full justify-between">
            <span class="text-gray-700 flex gap-1 items-center">
              Cell output size
            </span>
            <.select_field
              id="cell-output-size"
              name="output_size"
              aria-label="cell output size"
              value={@output_size}
              options={@output_size_options}
            />
          </div>
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
          "output_size" => output_size,
          "reevaluate_automatically" => reevaluate_automatically,
          "continue_on_error" => continue_on_error
        },
        socket
      ) do
    reevaluate_automatically = reevaluate_automatically == "true"
    continue_on_error = continue_on_error == "true"

    output_size =
      case output_size do
        "default" -> :default
        "wide" -> :wide
        "full" -> :full
      end

    Session.set_cell_attributes(socket.assigns.session.pid, socket.assigns.cell.id, %{
      output_size: output_size,
      reevaluate_automatically: reevaluate_automatically,
      continue_on_error: continue_on_error
    })

    {:noreply, push_patch(socket, to: socket.assigns.return_to)}
  end
end

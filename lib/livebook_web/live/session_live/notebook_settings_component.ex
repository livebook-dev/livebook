defmodule LivebookWeb.SessionLive.NotebookSettingsComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, changeset: nil, initialized: false)}
  end

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    socket =
      if socket.assigns.initialized do
        socket
      else
        {width_mode, custom_value, custom_unit} =
          case assigns.container_width do
            :default -> {:default, "1200", :px}
            :wide -> {:wide, "1200", :px}
            :full -> {:full, "1200", :px}
            {:custom, %{value: value, unit: unit}} -> {:custom, Integer.to_string(value), unit}
          end

        assign(socket,
          width_mode: width_mode,
          custom_value: custom_value,
          custom_unit: custom_unit,
          initialized: true
        )
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-5" data-el-notebook-settings>
      <h3 class="text-2xl font-semibold text-gray-800">
        Notebook settings
      </h3>
      <form>
        <div class="flex flex-col space-y-4">
          <div>
            <span class="text-sm font-semibold text-gray-700">
              Container width
            </span>
            <p class="text-sm text-gray-500 mb-2">
              Sets the maximum width of the notebook content. Applies to both the notebook editor and deployed apps.
            </p>
            <.select_field
              name="width_mode"
              value={@width_mode}
              label="Width"
              options={[
                {"Default (1024px)", "default"},
                {"Wide (1440px)", "wide"},
                {"Full width", "full"},
                {"Custom", "custom"}
              ]}
              phx-change="set_width_mode"
              phx-target={@myself}
            />
          </div>

          <div :if={@width_mode == :custom} class="flex flex-col space-y-2">
            <span class="text-sm font-semibold text-gray-700">
              Custom max width
            </span>
            <div class="flex space-x-2">
              <.text_field
                name="custom_value"
                value={@custom_value}
                phx-debounce="300"
                phx-keyup="set_custom_value"
                phx-target={@myself}
                class="flex-1"
              />
              <.select_field
                name="custom_unit"
                value={@custom_unit}
                options={[{"px", "px"}, {"%", "percent"}]}
                phx-change="set_custom_unit"
                phx-target={@myself}
                class="w-24"
              />
            </div>
            <p :if={@changeset && @changeset.errors[:custom_value]} class="text-sm text-red-500">
              Value must be a positive number
            </p>
          </div>
        </div>
      </form>
    </div>
    """
  end

  @impl true
  def handle_event("set_width_mode", %{"width_mode" => mode}, socket) do
    width_mode = String.to_atom(mode)
    socket = assign(socket, width_mode: width_mode, changeset: nil)

    container_width =
      case width_mode do
        :default -> :default
        :wide -> :wide
        :full -> :full
        :custom ->
          case Integer.parse(socket.assigns.custom_value) do
            {value, ""} when value > 0 ->
              {:custom, %{value: value, unit: socket.assigns.custom_unit}}

            _ ->
              {:custom, %{value: 1200, unit: :px}}
          end
      end

    send(self(), {:set_notebook_attributes, %{container_width: container_width}})

    {:noreply, socket}
  end

  def handle_event("set_custom_unit", %{"custom_unit" => unit}, socket) do
    custom_unit = String.to_atom(unit)
    socket = assign(socket, custom_unit: custom_unit)

    case Integer.parse(socket.assigns.custom_value) do
      {value, ""} when value > 0 ->
        container_width = {:custom, %{value: value, unit: custom_unit}}
        send(self(), {:set_notebook_attributes, %{container_width: container_width}})

      _ ->
        :ok
    end

    {:noreply, socket}
  end

  def handle_event("set_custom_value", %{"value" => custom_value}, socket) do
    changeset =
      case Integer.parse(custom_value) do
        {value, ""} when value > 0 ->
          %{valid?: true, errors: []}

        _ ->
          %{valid?: false, errors: [custom_value: "must be a positive number"]}
      end

    socket = assign(socket, custom_value: custom_value, changeset: changeset)

    if changeset.valid? do
      {value, ""} = Integer.parse(custom_value)
      container_width = {:custom, %{value: value, unit: socket.assigns.custom_unit}}
      send(self(), {:set_notebook_attributes, %{container_width: container_width}})
    end

    {:noreply, socket}
  end
end

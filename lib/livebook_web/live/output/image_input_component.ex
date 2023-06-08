defmodule LivebookWeb.Output.ImageInputComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, value: nil)}
  end

  @impl true
  def update(assigns, socket) do
    {value, assigns} = Map.pop!(assigns, :value)

    socket = assign(socket, assigns)

    socket =
      if value == socket.assigns.value do
        socket
      else
        image_info =
          if value do
            %{data: Base.encode64(value.data), height: value.height, width: value.width}
          end

        push_event(socket, "image_input_change:#{socket.assigns.id}", %{image_info: image_info})
      end

    {:ok, assign(socket, value: value)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id={"#{@id}-root"}
      class="inline-flex flex-col p-4 border-2 border-dashed border-gray-200 rounded-lg"
      phx-hook="ImageInput"
      phx-update="ignore"
      data-id={@id}
      data-phx-target={@myself}
      data-height={@height}
      data-width={@width}
      data-format={@format}
      data-fit={@fit}
    >
      <input type="file" data-input class="hidden" name="html_value" accept="image/*" capture="user" />
      <div class="flex justify-center" data-preview>
        <div class="flex justify-center text-gray-500">
          Drag an image file
        </div>
      </div>
      <div class="hidden flex justify-center" data-camera-preview></div>
      <div class="mt-4 flex items-center justify-center gap-4">
        <.menu id={"#{@id}-camera-select-menu"} position={:bottom_left}>
          <:toggle>
            <button
              class="button-base button-gray border-transparent py-2 px-4 inline-flex text-gray-500"
              data-btn-open-camera
            >
              <.remix_icon icon="camera-line" class="text-lg leading-none mr-2" />
              <span>Open camera</span>
            </button>
          </:toggle>
          <div data-camera-list>
            <.menu_item>
              <button role="menuitem" data-camera-id>
                <span class="font-medium" data-label></span>
              </button>
            </.menu_item>
          </div>
        </.menu>
        <button
          class="hidden button-base button-gray border-transparent py-2 px-4 inline-flex text-gray-500"
          data-btn-capture-camera
        >
          <.remix_icon icon="camera-line" class="text-lg leading-none mr-2" />
          <span>Take photo</span>
        </button>
        <button
          class="hidden button-base button-gray border-transparent py-2 px-4 inline-flex text-gray-500"
          data-btn-cancel
        >
          <.remix_icon icon="close-circle-line" class="text-lg leading-none mr-2" />
          <span>Cancel</span>
        </button>
        <button
          class="button-base button-gray border-transparent py-2 px-4 inline-flex text-gray-500"
          data-btn-upload
        >
          <.remix_icon icon="upload-2-line" class="text-lg leading-none mr-2" />
          <span>Upload</span>
        </button>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("change", params, socket) do
    value = %{
      data: Base.decode64!(params["data"]),
      height: params["height"],
      width: params["width"],
      format: socket.assigns.format
    }

    send_update(LivebookWeb.Output.InputComponent,
      id: socket.assigns.input_component_id,
      event: :change,
      value: value
    )

    {:noreply, socket}
  end
end

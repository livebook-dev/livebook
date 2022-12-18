defmodule LivebookWeb.Output.ImageInputComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, initialized: false)}
  end

  @impl true
  def update(assigns, socket) do
    {value, assigns} = Map.pop!(assigns, :value)

    socket = assign(socket, assigns)

    socket =
      if socket.assigns.initialized do
        socket
      else
        socket =
          if value do
            push_event(socket, "image_input_init:#{socket.assigns.id}", %{
              data: Base.encode64(value.data),
              height: value.height,
              width: value.width,
              format: value.format
            })
          else
            socket
          end

        assign(socket, initialize: true)
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id={"#{@id}-root"}
      class="inline-flex p-4 border-2 border-dashed border-gray-200 rounded-lg cursor-pointer"
      phx-hook="ImageInput"
      data-id={@id}
      data-phx-target={@target}
      data-height={@height}
      data-width={@width}
      data-format={@format}
      data-fit={@fit}
    >
      <input type="file" data-input class="hidden" name="value" accept="image/*" capture="user" />
      <div>
        <div id={"#{@id}-preview"} phx-update="ignore" data-preview>
          <div class="text-gray-500">
            Drag an image file here or click to open file browser
          </div>
        </div>
        <div class="flex items-center justify-center text-gray-500" data-from-camera="true" data-camera-select-menu>
          <.menu id={"#{@id}-camera-select-menu"} position="bottom-left">
            <:toggle>
              <button class="icon-button button-gray mt-2 rounded-lg" aria-label="select camera" data-from-camera="true">
                <span camera-button-label data-from-camera="true">Open camera</span>
                <.remix_icon icon="camera-switch-line" style="padding-inline-start: 5px" class="text-sm" />
              </button>
            </:toggle>
            <:content>
              <div data-camera-list></div>
            </:content>
          </.menu>
        </div>
      </div>
    </div>
    """
  end
end

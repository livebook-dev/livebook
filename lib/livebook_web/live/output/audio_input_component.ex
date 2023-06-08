defmodule LivebookWeb.Output.AudioInputComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, endianness: System.endianness(), value: nil)}
  end

  @impl true
  def update(assigns, socket) do
    {value, assigns} = Map.pop!(assigns, :value)

    socket = assign(socket, assigns)

    socket =
      if value == socket.assigns.value do
        socket
      else
        audio_info =
          if value do
            %{
              data: Base.encode64(value.data),
              num_channels: value.num_channels,
              sampling_rate: value.sampling_rate
            }
          end

        push_event(socket, "audio_input_change:#{socket.assigns.id}", %{audio_info: audio_info})
      end

    {:ok, assign(socket, value: value)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id={"#{@id}-root"}
      class="inline-flex flex-col gap-4 p-4 border-2 border-dashed border-gray-200 rounded-lg"
      phx-hook="AudioInput"
      phx-update="ignore"
      data-id={@id}
      data-phx-target={@myself}
      data-format={@format}
      data-sampling-rate={@sampling_rate}
      data-endianness={@endianness}
    >
      <input type="file" data-input class="hidden" name="html_value" accept="audio/*" capture="user" />
      <audio controls data-preview></audio>
      <div class="flex items-center justify-center gap-4">
        <button
          class="button-base button-gray border-transparent py-2 px-4 inline-flex text-gray-500"
          data-btn-record
        >
          <.remix_icon icon="mic-line" class="text-lg leading-none mr-2" />
          <span>Record</span>
        </button>
        <button
          class="hidden button-base button-gray border-transparent py-2 px-4 inline-flex text-gray-500 items-center"
          data-btn-stop
        >
          <span class="mr-2 flex h-3 w-3 relative">
            <span class="animate-ping absolute inline-flex h-full w-full rounded-full bg-gray-400 opacity-75">
            </span>
            <span class="relative inline-flex rounded-full h-3 w-3 bg-gray-500"></span>
          </span>
          <span>Stop recording</span>
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
      num_channels: params["num_channels"],
      sampling_rate: params["sampling_rate"],
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

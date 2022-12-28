defmodule LivebookWeb.Output.AudioInputComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, endianness: System.endianness(), initialized: false)}
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
            push_event(socket, "audio_input_init:#{socket.assigns.id}", %{
              data: Base.encode64(value.data),
              num_channels: value.num_channels,
              sampling_rate: value.sampling_rate
            })
          else
            socket
          end

        assign(socket, initialized: true)
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id={"#{@id}-root"}
      class="inline-flex flex-col p-4 border-2 border-dashed border-gray-200 rounded-lg"
      phx-hook="AudioInput"
      phx-update="ignore"
      data-id={@id}
      data-phx-target={@target}
      data-format={@format}
      data-sampling-rate={@sampling_rate}
      data-endianness={@endianness}
    >
      <input type="file" data-input class="hidden" name="value" accept="audio/*" capture="user" />
      <audio controls data-preview></audio>
      <div class="mt-4 flex items-center justify-center gap-4">
        <button
          class="button-base button-gray border-transparent py-2 px-4 inline-flex text-gray-500"
          data-btn-upload
        >
          <.remix_icon icon="upload-2-line" class="text-lg leading-none mr-2" />
          <span>Upload</span>
        </button>
        <button
          class="button-base button-gray border-transparent py-2 px-4 inline-flex text-gray-500"
          data-btn-record
        >
          <.remix_icon icon="mic-line" class="text-lg leading-none mr-2" />
          <span>Record</span>
        </button>
        <button
          class="hidden button-base button-gray border-transparent py-2 px-4 inline-flex text-gray-500"
          data-btn-stop
        >
          <.remix_icon icon="stop-line" class="text-lg leading-none mr-2" />
          <span>Stop recording</span>
        </button>
      </div>
    </div>
    """
  end
end

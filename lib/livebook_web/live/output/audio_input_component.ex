defmodule LivebookWeb.Output.AudioInputComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(
       endianness: System.endianness(),
       value: nil,
       audio_url: nil,
       decoding?: false
     )
     |> allow_upload(:file,
       accept: :any,
       max_entries: 1,
       max_file_size: 100_000_000_000,
       progress: &handle_progress/3,
       auto_upload: true
     )}
  end

  @impl true
  def update(assigns, socket) do
    {value, assigns} = Map.pop!(assigns, :value)

    socket = assign(socket, assigns)

    socket =
      cond do
        value == socket.assigns.value ->
          socket

        value == nil ->
          assign(socket, value: value, audio_url: nil)

        true ->
          assign(socket, value: value, audio_url: audio_url(socket.assigns.input_id))
      end

    {:ok, socket}
  end

  defp audio_url(input_id) do
    # For the client-side audio preview, we serve audio encoded as WAV
    # from a separate endpoint. To do that, we encode information in a
    # token and then the controller fetches input value from the LV.
    # This is especially important for client-specific inputs in forms.
    token = LivebookWeb.SessionHelpers.generate_input_token(self(), input_id)
    ~p"/public/sessions/audio-input/#{token}"
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="inline-flex flex-col gap-4 p-4 border-2 border-dashed border-gray-200 rounded-lg">
      <div
        class="inline-flex flex-col gap-4"
        id={"#{@id}-root"}
        phx-hook="AudioInput"
        phx-update="ignore"
        data-p-id={hook_prop(@id)}
        data-p-phx-target={hook_prop(@myself)}
        data-p-format={hook_prop(@format)}
        data-p-sampling-rate={hook_prop(@sampling_rate)}
        data-p-endianness={hook_prop(@endianness)}
        data-p-audio-url={hook_prop(@audio_url)}
      >
        <input
          type="file"
          data-input
          class="hidden"
          name="html_value"
          accept="audio/*"
          capture="user"
        />
        <audio controls data-preview></audio>
        <div class="flex items-center justify-center gap-4">
          <.button color="gray" data-btn-record>
            <.remix_icon icon="mic-line" />
            <span>Record</span>
          </.button>
          <.button color="gray" class="hidden" data-btn-stop>
            <span class="flex h-2 w-2 relative">
              <span class="animate-ping absolute inline-flex h-full w-full rounded-full bg-gray-400 opacity-75">
              </span>
              <span class="relative inline-flex rounded-full h-2 w-2 bg-gray-500"></span>
            </span>
            <span>Stop recording</span>
          </.button>
          <.button color="gray" class="hidden" data-btn-cancel>
            <.remix_icon icon="close-circle-line" />
            <span>Cancel</span>
          </.button>
          <.button color="gray" data-btn-upload>
            <.remix_icon icon="upload-2-line" />
            <span>Upload</span>
          </.button>
        </div>
      </div>
      <form phx-change="validate" class="hidden" phx-target={@myself}>
        <.live_file_input upload={@uploads.file} />
      </form>
      <div
        :if={@uploads.file.entries == [] and @decoding?}
        class="delay-200 flex justify-center items-center gap-2"
      >
        <span class="text-sm font-medium text-gray-500">Decoding</span> <.spinner />
      </div>
      <div :for={entry <- @uploads.file.entries} class="delay-200 flex flex-col gap-1">
        <.file_entry name="Audio" entry={entry} on_clear={JS.push("clear_file", target: @myself)} />
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("decoding", %{}, socket) do
    {:noreply, assign(socket, decoding?: true)}
  end

  def handle_event("validate", %{}, socket) do
    {:noreply, socket}
  end

  def handle_event("clear_file", %{"ref" => ref}, socket) do
    {:noreply, cancel_upload(socket, :file, ref)}
  end

  defp handle_progress(:file, entry, socket) do
    if entry.done? do
      file_ref =
        consume_uploaded_entry(socket, entry, fn %{path: path} ->
          {:ok, file_ref} =
            LivebookWeb.SessionHelpers.register_input_file(
              socket.assigns.session_pid,
              path,
              socket.assigns.input_id,
              socket.assigns.local,
              socket.assigns.client_id
            )

          {:ok, file_ref}
        end)

      %{"num_channels" => num_channels, "sampling_rate" => sampling_rate} = entry.client_meta

      value = %{
        file_ref: file_ref,
        num_channels: num_channels,
        sampling_rate: sampling_rate,
        format: socket.assigns.format
      }

      send_update(LivebookWeb.Output.InputComponent,
        id: socket.assigns.input_component_id,
        event: :change,
        value: value
      )
    end

    {:noreply, assign(socket, decoding?: false)}
  end
end

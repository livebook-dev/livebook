defmodule LivebookWeb.Output.ImageInputComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(value: nil, value: nil, image_url: nil)
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
          assign(socket, value: value, image_url: nil)

        true ->
          assign(socket, value: value, image_url: image_url(socket.assigns.input_id))
      end

    {:ok, socket}
  end

  defp image_url(input_id) do
    # For the client-side image preview, we serve the original binary
    # value from a separate endpoint. To do that, we encode information
    # in a token and then the controller fetches input value from the
    # LV. This is especially important for client-specific inputs in
    # forms.
    token = LivebookWeb.SessionHelpers.generate_input_token(self(), input_id)
    ~p"/public/sessions/image-input/#{token}"
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="inline-flex flex-col gap-4 p-4 border-2 border-dashed border-gray-200 rounded-lg">
      <div
        id={"#{@id}-root"}
        class="inline-flex flex-col"
        phx-hook="ImageInput"
        phx-update="ignore"
        data-p-id={hook_prop(@id)}
        data-p-phx-target={hook_prop(@myself)}
        data-p-height={hook_prop(@height)}
        data-p-width={hook_prop(@width)}
        data-p-format={hook_prop(@format)}
        data-p-fit={hook_prop(@fit)}
        data-p-image-url={hook_prop(@image_url)}
        data-p-value-height={hook_prop(@value[:height])}
        data-p-value-width={hook_prop(@value[:width])}
      >
        <input
          type="file"
          data-input
          class="hidden"
          name="html_value"
          accept="image/*"
          capture="user"
        />
        <div class="flex justify-center" data-preview>
          <div class="flex justify-center text-gray-500">
            Drag an image file
          </div>
        </div>
        <div class="hidden flex justify-center" data-camera-preview></div>
        <div class="mt-4 flex items-center justify-center gap-4">
          <.menu id={"#{@id}-camera-select-menu"} position={:bottom_left}>
            <:toggle>
              <.button color="gray" data-btn-open-camera>
                <.remix_icon icon="camera-line" />
                <span>Open camera</span>
              </.button>
            </:toggle>
            <div data-camera-list>
              <.menu_item>
                <button role="menuitem" data-camera-id>
                  <span class="font-medium" data-label></span>
                </button>
              </.menu_item>
            </div>
          </.menu>
          <.button color="gray" class="hidden" data-btn-capture-camera>
            <.remix_icon icon="camera-line" />
            <span>Take photo</span>
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
      <div :for={entry <- @uploads.file.entries} class="delay-200 flex flex-col gap-1">
        <.file_entry name="Image" entry={entry} on_clear={JS.push("clear_file", target: @myself)} />
      </div>
    </div>
    """
  end

  @impl true
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

      %{"height" => height, "width" => width} = entry.client_meta

      value = %{
        file_ref: file_ref,
        height: height,
        width: width,
        format: socket.assigns.format
      }

      send_update(LivebookWeb.Output.InputComponent,
        id: socket.assigns.input_component_id,
        event: :change,
        value: value
      )
    end

    {:noreply, socket}
  end
end

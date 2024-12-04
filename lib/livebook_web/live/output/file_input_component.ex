defmodule LivebookWeb.Output.FileInputComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, initialized: false)}
  end

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    socket =
      if socket.assigns.initialized do
        socket
      else
        socket
        |> allow_upload(:file,
          # The file input specifies the accepted formats, but in order
          # to handle unknown MIME types, we need to pass :any to
          # allow_upload and override the accept attribute ourselves
          accept: :any,
          max_entries: 1,
          max_file_size: 100_000_000_000,
          progress: &handle_progress/3,
          auto_upload: true
        )
        |> assign(
          initialized: true,
          accept:
            case socket.assigns.accept do
              :any -> nil
              types when is_list(types) -> Enum.join(types, ",")
            end
        )
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <form id={"#{@id}-root"} phx-change="validate" phx-target={@myself}>
      <label
        class="min-w-[50%] inline-flex flex-col gap-4 p-4 border-2 border-dashed border-gray-200 rounded-lg cursor-pointer"
        phx-drop-target={@uploads.file.ref}
        phx-hook="Dropzone"
        id={"#{@id}-upload-dropzone"}
      >
        <div class="flex justify-center text-gray-500">
          <%= if @value do %>
            {@value.client_name}
          <% else %>
            Click to select a file or drag a local file here
          <% end %>
        </div>
        <.live_file_input upload={@uploads.file} class="hidden" accept={@accept} />
        <div :for={entry <- @uploads.file.entries} class="delay-200 flex flex-col gap-1">
          <.file_entry name="File" entry={entry} on_clear={JS.push("clear_file", target: @myself)} />
        </div>
      </label>
      <p
        :for={msg <- LivebookWeb.HTMLHelpers.upload_error_messages(@uploads.file)}
        class="mt-0.5 text-red-600 text-sm"
      >
        {msg}
      </p>
    </form>
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
      {file_ref, client_name} =
        consume_uploaded_entry(socket, entry, fn %{path: path} ->
          {:ok, file_ref} =
            LivebookWeb.SessionHelpers.register_input_file(
              socket.assigns.session_pid,
              path,
              socket.assigns.input_id,
              socket.assigns.local,
              socket.assigns.client_id
            )

          {:ok, {file_ref, entry.client_name}}
        end)

      value = %{file_ref: file_ref, client_name: client_name}

      send_update(LivebookWeb.Output.InputComponent,
        id: socket.assigns.input_component_id,
        event: :change,
        value: value
      )
    end

    {:noreply, socket}
  end
end

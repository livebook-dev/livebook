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
          accept: socket.assigns.accept,
          max_entries: 1,
          progress: &handle_progress/3,
          auto_upload: true
        )
        |> assign(initialized: true)
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <form id={"#{@id}-root"} phx-change="validate" phx-target={@myself}>
      <label
        class="inline-flex flex-col gap-4 p-4 border-2 border-dashed border-gray-200 rounded-lg cursor-pointer"
        phx-drop-target={@uploads.file.ref}
        phx-hook="Dropzone"
        id="upload-file-dropzone"
      >
        <div class="flex justify-center text-gray-500">
          <%= if @value do %>
            <%= @value.client_name %>
          <% else %>
            Click to select a file or drag a local file here
          <% end %>
        </div>
        <.live_file_input upload={@uploads.file} class="hidden" />
      </label>
    </form>
    """
  end

  @impl true
  def handle_event("validate", %{}, socket) do
    {:noreply, socket}
  end

  defp handle_progress(:file, entry, socket) do
    if entry.done? do
      socket
      |> consume_uploaded_entries(:file, fn %{path: path}, entry ->
        destination_path =
          Livebook.Session.local_file_input_path(
            socket.assigns.session_id,
            socket.assigns.input_id
          )

        destination_path |> Path.dirname() |> File.mkdir_p!()
        File.cp!(path, destination_path)

        {:ok, {destination_path, entry.client_name}}
      end)
      |> case do
        [{path, client_name}] ->
          # The path is always the same, so we include a random version
          # to reflect a new value
          value = %{
            path: path,
            client_name: client_name,
            version: Livebook.Utils.random_short_id()
          }

          send_update(LivebookWeb.Output.InputComponent,
            id: socket.assigns.input_component_id,
            event: :change,
            value: value
          )

        [] ->
          :ok
      end
    end

    {:noreply, socket}
  end
end

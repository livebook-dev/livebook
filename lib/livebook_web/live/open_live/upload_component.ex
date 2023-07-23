defmodule LivebookWeb.OpenLive.UploadComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(:error, false)
     |> allow_upload(:file, accept: ~w(.livemd), max_entries: 1)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex-col space-y-5">
      <p class="text-gray-700" id="import-from-file">
        Drag and drop a .livemd file below to import it.
      </p>
      <form id="upload-file-form" phx-submit="save" phx-change="validate" phx-target={@myself}>
        <div class="flex flex-col space-y-4">
          <.file_drop_input
            upload={@uploads.file}
            label="Notebook"
            on_clear={JS.push("clear_file", target: @myself)}
          />
        </div>
        <%= if @error do %>
          <div class="text-red-500 text-sm py-2">
            You can only upload files with .livemd extension.
          </div>
        <% end %>
        <button
          type="submit"
          class="mt-5 button-base button-blue"
          disabled={@error or upload_disabled?(@uploads.file)}
        >
          Import
        </button>
      </form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", _params, socket) do
    has_error? = Enum.any?(socket.assigns.uploads.file.entries, &(not &1.valid?))

    {:noreply, assign(socket, error: has_error?)}
  end

  def handle_event("clear_file", %{"ref" => ref}, socket) do
    {:noreply,
     socket
     |> cancel_upload(:file, ref)
     |> assign(error: false)}
  end

  def handle_event("save", _params, socket) do
    consume_uploaded_entries(socket, :file, fn %{path: path}, _entry ->
      content = File.read!(path)

      send(self(), {:import_source, content, []})

      {:ok, :ok}
    end)

    {:noreply, socket}
  end
end

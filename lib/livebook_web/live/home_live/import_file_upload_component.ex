defmodule LivebookWeb.HomeLive.ImportFileUploadComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(:error, false)
     |> allow_upload(:notebook, accept: ~w(.livemd), max_entries: 1)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex-col space-y-5">
      <p class="text-gray-700" id="import-from-file">
        Drag and drop a <code>.livemd</code> file below to import it.
      </p>
      <form id="upload-file-form"
        phx-submit="save"
        phx-change="validate"
        phx-drop-target={@uploads.notebook.ref}
        phx-target={@myself}
        phx-hook="DragAndDrop"
        class="flex flex-col items-start"
      >
        <%= live_file_input @uploads.notebook, class: "hidden", aria_labelledby: "import-from-file" %>
        <div data-dropzone class="flex flex-col justify-center items-center w-full rounded-xl border-2 border-dashed border-gray-400 h-48">
          <%= if @uploads.notebook.entries == [] do %>
            <span name="placeholder" class="font-medium text-gray-400">Drop your notebook here</span>
          <% else %>
            <%= for file <- @uploads.notebook.entries do %>
              <div class="flex items-center">
                <span class="font-medium text-gray-400"><%= file.client_name %></span>
                <button type="button" class="icon-button" phx-click="clear-file" phx-target={@myself} tabindex="-1">
                  <.remix_icon icon="close-line" class="text-xl text-gray-300 hover:text-gray-500" />
                </button>
              </div>
            <% end %>
          <% end %>
        </div>
        <%= if @error do %>
          <div class="text-red-500 text-sm py-2">
            You can only upload files with .livemd extension.
          </div>
        <% end %>
        <button type="submit" class="mt-5 button-base button-blue" disabled={@uploads.notebook.entries == [] || @error}>
          Import
        </button>
      </form>
    </div>
    """
  end

  @impl Phoenix.LiveComponent
  def handle_event("clear-file", _params, socket) do
    {socket, _entries} = Phoenix.LiveView.Upload.maybe_cancel_uploads(socket)
    {:noreply, assign(socket, error: false)}
  end

  @impl Phoenix.LiveComponent
  def handle_event("validate", _params, socket) do
    has_error? = Enum.any?(socket.assigns.uploads.notebook.entries, &(not &1.valid?))

    {:noreply, assign(socket, error: has_error?)}
  end

  @impl Phoenix.LiveComponent
  def handle_event("save", _params, socket) do
    consume_uploaded_entries(socket, :notebook, fn %{path: path}, _entry ->
      content = File.read!(path)

      send(self(), {:import_content, content, []})
    end)

    {:noreply, socket}
  end
end

defmodule LivebookWeb.SessionLive.CellUploadComponent do
  use LivebookWeb, :live_component

  alias Livebook.FileSystem

  @impl true
  def mount(socket) do
    {:ok, assign(socket, name: "", error_message: nil)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 pb-4 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Insert image
      </h3>
      <%= if @uploads.cell_image.errors != [] do %>
        <div class="error-box">
          Invalid image file. The image must be either GIF, JPEG, or PNG and cannot exceed 5MB in size.
        </div>
      <% end %>
      <%= if @error_message do %>
        <div class="error-box">
          <%= @error_message %>
        </div>
      <% end %>
      <%= for entry <- @uploads.cell_image.entries do %>
        <div class="flex flex-col space-y-1">
          <div class="flex justify-between text-gray-700">
            <span><%= entry.client_name %></span>
            <span><%= entry.progress %>%</span>
          </div>
          <div class="w-full h-2 rounded-lg bg-blue-200">
            <div class="h-full rounded-lg bg-blue-600 transition-all ease-out duration-1000"
              style={"width: #{entry.progress}%"}>
            </div>
          </div>
        </div>
      <% end %>
      <form phx-submit="save" phx-change="validate" phx-target={@myself}>
        <div class="w-full flex space-x-2">
          <div>
            <label>
              <%= live_file_input @uploads.cell_image, class: "hidden" %>
              <div class="cursor-pointer button-base button-gray button-square-icon">
                <.remix_icon icon="folder-upload-line" />
              </div>
            </label>
          </div>
          <div class="grow">
            <input class="input" name="name" value={@name} placeholder="Name" autocomplete="off" />
          </div>
        </div>
        <div class="mt-8 flex justify-end space-x-2">
          <%= live_patch "Cancel", to: @return_to, class: "button-base button-outlined-gray" %>
          <button class="button-base button-blue"
            type="submit"
            disabled={@uploads.cell_image.entries == [] or @name == ""}>
            Upload
          </button>
        </div>
      </form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"name" => name}, socket) do
    {:noreply, assign(socket, name: name)}
  end

  def handle_event("save", %{"name" => name}, socket) do
    %{images_dir: images_dir} = socket.assigns.session

    consume_uploaded_entries(socket, :cell_image, fn %{path: path}, entry ->
      # Ensure the path is normalized (see https://github.com/elixir-plug/plug/issues/1047)
      # TODO: remove once we update to a Plug version with the issue resolved
      path = Path.expand(path)
      upload_file = FileSystem.File.local(path)
      ext = Path.extname(entry.client_name)
      filename = name <> ext
      destination_file = FileSystem.File.resolve(images_dir, filename)

      result =
        with :ok <- FileSystem.File.copy(upload_file, destination_file) do
          {:ok, filename}
        end

      {:ok, result}
    end)
    |> case do
      [{:ok, filename}] ->
        src_path = "images/#{URI.encode(filename, &URI.char_unreserved?/1)}"

        {:noreply,
         socket
         |> push_patch(to: socket.assigns.return_to)
         |> push_event("cell_upload", %{cell_id: socket.assigns.cell.id, url: src_path})}

      [{:error, message}] ->
        {:noreply, assign(socket, error_message: message)}
    end
  end
end

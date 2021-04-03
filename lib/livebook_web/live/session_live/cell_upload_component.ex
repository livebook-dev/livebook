defmodule LivebookWeb.SessionLive.CellUploadComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  @impl true
  def mount(socket) do
    {:ok, assign(socket, name: "")}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="p-6 pb-4 max-w-xl w-screen flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Insert image
      </h3>
      <%= for entry <- @uploads.cell_image.entries do %>
        <div class="flex flex-col space-y-1">
          <div class="flex justify-between text-gray-700">
            <span><%= entry.client_name %></span>
            <span><%= entry.progress %>%</span>
          </div>
          <div class="w-full h-2 rounded-lg bg-blue-200">
            <div class="h-full rounded-lg bg-blue-600 transition-all ease-out duration-1000"
              style="width: <%= entry.progress %>%">
            </div>
          </div>
        </div>
      <% end %>
      <form phx-submit="save" phx-change="validate" phx-target="<%= @myself %>">
        <div class="w-full flex space-x-2">
          <div>
            <label>
              <%= live_file_input @uploads.cell_image, class: "hidden" %>
              <div class="inline-block cursor-pointer button button-gray button-square-icon">
                <%= remix_icon("folder-upload-line") %>
              </div>
            </label>
          </div>
          <div class="flex-grow">
            <input class="input" name="name" placeholder="Name" autocomplete="off" value="<%= @name %>" />
          </div>
        </div>
        <div class="mt-8 flex justify-end space-x-2">
          <%= live_patch "Cancel", to: @return_to, class: "button button-outlined-gray" %>
          <%= content_tag :button, "Upload",
                type: :submit,
                class: "button button-blue",
                disabled: @uploads.cell_image.entries == [] or @name == "" %>
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
    consume_uploaded_entries(socket, :cell_image, fn %{path: path}, _entry ->
      # todo: ext
      dest = Path.join("/tmp", name)
      File.cp!(path, dest)
    end)

    # todo: add ext
    path = "./images/#{name}"

    {:noreply,
     socket
     |> push_patch(to: socket.assigns.return_to)
     |> push_event("cell_upload", %{cell_id: socket.assigns.cell.id, url: name})}
  end
end

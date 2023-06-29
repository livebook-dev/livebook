defmodule LivebookWeb.SessionLive.InsertImageComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  alias Livebook.FileSystem

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(changeset: changeset(), error_message: nil)
     |> allow_upload(:image,
       accept: ~w(.jpg .jpeg .png .gif .svg),
       max_entries: 1,
       max_file_size: 5_000_000
     )}
  end

  defp changeset(attrs \\ %{}) do
    data = %{name: nil}
    types = %{name: :string}

    cast({data, types}, attrs, [:name])
    |> validate_required([:name])
    |> Livebook.Notebook.validate_file_entry_name(:name)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Insert image
      </h3>
      <div :if={@uploads.image.errors != []} class="error-box">
        Invalid image file. The image must be either GIF, JPEG, SVG or PNG and cannot exceed 5MB in size.
      </div>
      <div :if={@error_message} class="error-box">
        <%= @error_message %>
      </div>
      <div :for={entry <- @uploads.image.entries} class="flex flex-col space-y-1">
        <div class="flex justify-between text-gray-700">
          <span><%= entry.client_name %></span>
          <span><%= entry.progress %>%</span>
        </div>
        <div class="w-full h-2 rounded-lg bg-blue-200">
          <div
            class="h-full rounded-lg bg-blue-600 transition-all ease-out duration-1000"
            style={"width: #{entry.progress}%"}
          >
          </div>
        </div>
      </div>
      <.form
        :let={f}
        for={@changeset}
        as={:data}
        phx-change="validate"
        phx-submit="save"
        phx-target={@myself}
      >
        <div class="w-full flex space-x-2">
          <div>
            <label>
              <.live_file_input upload={@uploads.image} class="hidden" />
              <div class="cursor-pointer button-base button-gray button-square-icon">
                <.remix_icon icon="folder-upload-line" />
              </div>
            </label>
          </div>
          <div class="grow">
            <.text_field field={f[:name]} placeholder="Name" autocomplete="off" phx-debounce="blur" />
          </div>
        </div>
        <div class="mt-8 flex justify-end space-x-2">
          <.link patch={@return_to} class="button-base button-outlined-gray">
            Cancel
          </.link>
          <button
            class="button-base button-blue"
            type="submit"
            disabled={
              not @changeset.valid? or @uploads.image.entries == [] or
                @uploads.image.errors != []
            }
          >
            Upload
          </button>
        </div>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("yo", %{}, socket) do
    {:noreply, socket}
  end

  def handle_event("validate", %{"data" => data} = params, socket) do
    upload_entries = socket.assigns.uploads.image.entries

    data =
      case {params["_target"], data["name"], upload_entries} do
        {["image"], "", [entry]} ->
          %{data | "name" => entry.client_name}

        _ ->
          data
      end

    changeset = data |> changeset() |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("save", %{"data" => data}, socket) do
    data
    |> changeset()
    |> apply_action(:insert)
    |> case do
      {:ok, data} ->
        %{files_dir: files_dir} = socket.assigns.session

        [upload_result] =
          consume_uploaded_entries(socket, :image, fn %{path: path}, _entry ->
            upload_file = FileSystem.File.local(path)
            destination_file = FileSystem.File.resolve(files_dir, data.name)

            result =
              with :ok <- FileSystem.File.copy(upload_file, destination_file) do
                {:ok, data.name}
              end

            {:ok, result}
          end)

        case upload_result do
          {:ok, filename} ->
            file_entry = %{name: filename, type: :attachment}
            Livebook.Session.add_file_entries(socket.assigns.session.pid, [file_entry])
            url = "files/#{URI.encode(filename, &URI.char_unreserved?/1)}"
            send(self(), {:insert_image_complete, socket.assigns.insert_image_metadata, url})
            {:noreply, push_patch(socket, to: socket.assigns.return_to)}

          {:error, message} ->
            {:noreply, assign(socket, error_message: message)}
        end

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end
end

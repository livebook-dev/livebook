defmodule LivebookWeb.SessionLive.InsertImageComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  alias Livebook.FileSystem

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(changeset: changeset())
     |> allow_upload(:image,
       accept: ~w(.jpg .jpeg .png .gif .svg),
       max_entries: 1,
       max_file_size: 5_000_000,
       writer: fn _name, _entry, socket ->
         file = file_entry_file(socket)
         {LivebookWeb.FileSystemWriter, [file: file]}
       end
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
      <div class="flex flex-col gap-2">
        <div :for={message <- upload_error_messages(@uploads.image)} class="error-box">
          <%= message %>
        </div>
      </div>
      <div :for={entry <- @uploads.image.entries}>
        <.live_img_preview entry={entry} class="max-h-80 m-auto" />
      </div>
      <.form
        :let={f}
        for={@changeset}
        as={:data}
        phx-change="validate"
        phx-submit="save"
        phx-target={@myself}
      >
        <div class="flex flex-col space-y-4">
          <.file_drop_input
            upload={@uploads.image}
            label="File"
            on_clear={JS.push("clear_file", target: @myself)}
          />
          <.text_field
            field={f[:name]}
            label="Name"
            id="insert-image-form-name"
            autocomplete="off"
            phx-debounce="200"
          />
        </div>
        <div class="mt-8 flex justify-end space-x-2">
          <.link patch={@return_to} class="button-base button-outlined-gray">
            Cancel
          </.link>
          <button
            class="button-base button-blue"
            type="submit"
            disabled={not @changeset.valid? or upload_disabled?(@uploads.image)}
          >
            Upload
          </button>
        </div>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"data" => data} = params, socket) do
    upload_entries = socket.assigns.uploads.image.entries

    {data, socket} =
      case {params["_target"], data["name"], upload_entries} do
        {["image"], "", [entry]} ->
          # Emulate input event to make sure validation errors are shown
          socket =
            exec_js(
              socket,
              JS.dispatch("input", to: "#insert-image-form-name")
              |> JS.dispatch("blur", to: "#insert-image-form-name")
            )

          {%{data | "name" => entry.client_name}, socket}

        _ ->
          {data, socket}
      end

    changeset = data |> changeset() |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("clear_file", %{"ref" => ref}, socket) do
    {:noreply, cancel_upload(socket, :image, ref)}
  end

  def handle_event("save", %{"data" => data}, socket) do
    data
    |> changeset()
    |> apply_action(:insert)
    |> case do
      {:ok, data} ->
        [:ok] =
          consume_uploaded_entries(socket, :image, fn %{}, _entry -> {:ok, :ok} end)

        file_entry = %{name: data.name, type: :attachment}
        Livebook.Session.add_file_entries(socket.assigns.session.pid, [file_entry])
        url = "files/#{URI.encode(data.name, &URI.char_unreserved?/1)}"
        send(self(), {:insert_image_complete, socket.assigns.insert_image_metadata, url})
        {:noreply, push_patch(socket, to: socket.assigns.return_to)}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  defp file_entry_file(socket) do
    data = apply_changes(socket.assigns.changeset)
    FileSystem.File.resolve(socket.assigns.session.files_dir, data.name)
  end
end

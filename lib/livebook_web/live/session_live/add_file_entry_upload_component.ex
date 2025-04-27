defmodule LivebookWeb.SessionLive.AddFileEntryUploadComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset
  import LivebookWeb.HTMLHelpers

  alias Livebook.FileSystem

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(changeset: changeset())
     |> allow_upload(:file,
       accept: :any,
       max_entries: 1,
       max_file_size: 100_000_000_000,
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
    <div>
      <div :if={upload_error_messages(@uploads.file) != []} class="mb-6 flex flex-col gap-2">
        <div :for={message <- upload_error_messages(@uploads.file)} class="error-box">
          {message}
        </div>
      </div>
      <div class="mb-6">
        <.message_box kind="info">
          This will upload the file as an attachment in the notebook files/ directory.
          If you want to upload it to a file storage instead, choose <.link
            class="text-blue-600 "
            patch={~p"/sessions/#{@session.id}/add-file/storage"}
          >"From storage"</.link>,
          then drag and drop the file to the desired storage and directory.
        </.message_box>
      </div>
      <.form
        :let={f}
        for={@changeset}
        as={:data}
        id="add-file-entry-form"
        phx-change="validate"
        phx-submit="add"
        phx-target={@myself}
      >
        <div class="flex flex-col space-y-4">
          <.file_drop_input
            upload={@uploads.file}
            label="File"
            on_clear={JS.push("clear_file", target: @myself)}
          />
          <.text_field
            field={f[:name]}
            label="Name"
            id="add-file-entry-form-name"
            autocomplete="off"
            phx-debounce="200"
          />
        </div>
        <div class="mt-6 flex space-x-3">
          <.button type="submit" disabled={not @changeset.valid? or upload_disabled?(@uploads.file)}>
            <.spinner class="hidden phx-submit-loading:block mr-1" />
            <span>Add</span>
          </.button>
          <.button color="gray" outlined patch={~p"/sessions/#{@session.id}"}>
            Cancel
          </.button>
        </div>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"data" => data} = params, socket) do
    upload_entries = socket.assigns.uploads.file.entries

    {data, socket} =
      case {params["_target"], data["name"], upload_entries} do
        {["file"], "", [entry]} ->
          # Emulate input event to make sure validation errors are shown
          socket =
            exec_js(
              socket,
              JS.dispatch("input", to: "#add-file-entry-form-name")
              |> JS.dispatch("blur", to: "#add-file-entry-form-name")
            )

          name = LivebookWeb.SessionHelpers.sanitize_file_entry_name(entry.client_name)
          {%{data | "name" => name}, socket}

        _ ->
          {data, socket}
      end

    changeset = data |> changeset() |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("clear_file", %{"ref" => ref}, socket) do
    {:noreply, cancel_upload(socket, :file, ref)}
  end

  def handle_event("add", %{"data" => data}, socket) do
    data
    |> changeset()
    |> apply_action(:insert)
    |> case do
      {:ok, data} ->
        [:ok] =
          consume_uploaded_entries(socket, :file, fn %{}, _entry -> {:ok, :ok} end)

        file_entry = %{name: data.name, type: :attachment}
        Livebook.Session.add_file_entries(socket.assigns.session.pid, [file_entry])
        send(self(), {:file_entry_uploaded, file_entry})
        {:noreply, socket}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  defp file_entry_file(socket) do
    data = apply_changes(socket.assigns.changeset)
    FileSystem.File.resolve(socket.assigns.session.files_dir, data.name)
  end
end

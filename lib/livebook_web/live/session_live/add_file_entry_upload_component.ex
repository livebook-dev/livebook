defmodule LivebookWeb.SessionLive.AddFileEntryUploadComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  alias Livebook.FileSystem

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(changeset: changeset(), error_message: nil)
     |> allow_upload(:file, accept: :any, max_entries: 1, max_file_size: 100_000_000_000)}
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
      <div :if={@error_message} class="mb-6 error-box">
        <%= @error_message %>
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
          <.text_field field={f[:name]} label="Name" autocomplete="off" phx-debounce="blur" />
        </div>
        <div class="mt-6 flex space-x-3">
          <button
            class="button-base button-blue"
            type="submit"
            disabled={not @changeset.valid? or upload_disabled?(@uploads.file)}
          >
            Add
          </button>
          <.link patch={~p"/sessions/#{@session.id}"} class="button-base button-outlined-gray">
            Cancel
          </.link>
        </div>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"data" => data} = params, socket) do
    upload_entries = socket.assigns.uploads.file.entries

    data =
      case {params["_target"], data["name"], upload_entries} do
        {["file"], "", [entry]} ->
          %{data | "name" => entry.client_name}

        _ ->
          data
      end

    changeset = data |> changeset() |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("clear_file", %{"ref" => ref}, socket) do
    {:noreply,
     socket
     |> cancel_upload(:file, ref)
     |> assign(error_message: nil)}
  end

  def handle_event("add", %{"data" => data}, socket) do
    data
    |> changeset()
    |> apply_action(:insert)
    |> case do
      {:ok, data} ->
        %{files_dir: files_dir} = socket.assigns.session

        [upload_result] =
          consume_uploaded_entries(socket, :file, fn %{path: path}, _entry ->
            upload_file = FileSystem.File.local(path)
            destination_file = FileSystem.File.resolve(files_dir, data.name)
            result = FileSystem.File.copy(upload_file, destination_file)
            {:ok, result}
          end)

        case upload_result do
          :ok ->
            file_entry = %{name: data.name, type: :attachment}
            Livebook.Session.add_file_entries(socket.assigns.session.pid, [file_entry])
            send(self(), {:file_entry_uploaded, file_entry})
            {:noreply, push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}")}

          {:error, message} ->
            {:noreply, assign(socket, error_message: message)}
        end

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end
end

defmodule LivebookWeb.SessionLive.AddFileEntryFileComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  alias Livebook.FileSystem

  @impl true
  def mount(socket) do
    {:ok,
     assign(socket,
       file_info: %{exists: true},
       changeset: changeset(),
       error_message: nil,
       fetching: false
     )}
  end

  defp changeset(attrs \\ %{}) do
    data = %{name: nil, copy: false}
    types = %{name: :string, copy: :boolean}

    cast({data, types}, attrs, [:name, :copy])
    |> validate_required([:name, :copy])
    |> Livebook.Notebook.validate_file_entry_name(:name)
  end

  @impl true
  def update(%{event: {:set_file, file, info}}, socket) do
    file_info = %{exists: info.exists}
    name = if FileSystem.File.dir?(file), do: "", else: FileSystem.File.name(file)
    name = LivebookWeb.SessionHelpers.sanitize_file_entry_name(name)
    changeset = changeset(Map.put(socket.assigns.changeset.params, "name", name))
    {:ok, assign(socket, file: file, file_info: file_info, changeset: changeset)}
  end

  def update(assigns, socket) do
    {:ok,
     socket
     |> assign(assigns)
     |> assign_new(:file, fn -> Livebook.Settings.default_dir(assigns.hub) end)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col">
      <div :if={@error_message} class="mb-6 error-box">
        {@error_message}
      </div>
      <div class="h-80" role="region" aria-label="file storage">
        <.live_component
          module={LivebookWeb.FileSelectComponent}
          id="add-file-entry-select"
          file={@file}
          hub={@hub}
          extnames={:any}
          running_files={[]}
          target={{__MODULE__, @id}}
        >
        </.live_component>
      </div>
      <.form
        :let={f}
        for={@changeset}
        as={:data}
        class="mt-4"
        id="add-file-entry-form"
        phx-change="validate"
        phx-submit="add"
        phx-target={@myself}
      >
        <div class="flex flex-col space-y-4">
          <.text_field field={f[:name]} label="Name" autocomplete="off" phx-debounce="200" />
          <.radio_field
            field={f[:copy]}
            options={[
              {"false", "Store only file location"},
              {"true", "Save file as an attachment in the notebook files directory"}
            ]}
          />
        </div>
        <div class="mt-6 flex space-x-3">
          <.button
            type="submit"
            disabled={not @changeset.valid? or not regular?(@file, @file_info) or @fetching}
          >
            <.spinner :if={@fetching} class="mr-1" />
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
  def handle_event("validate", %{"data" => data}, socket) do
    changeset = data |> changeset() |> Map.replace!(:action, :validate)
    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("add", %{"data" => data}, socket) do
    data
    |> changeset()
    |> apply_action(:insert)
    |> case do
      {:ok, data} ->
        file_entry = %{name: data.name, type: :file, file: socket.assigns.file}

        if data.copy do
          session = socket.assigns.session

          socket =
            start_async(socket, :create_attachment_file_entry, fn ->
              Livebook.Session.to_attachment_file_entry(session, file_entry)
            end)

          {:noreply, assign(socket, fetching: true, error_message: nil)}
        else
          {:noreply, add_file_entry(socket, file_entry)}
        end

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  @impl true
  def handle_async(:create_attachment_file_entry, {:ok, file_entry_result}, socket) do
    socket = assign(socket, fetching: false)

    case file_entry_result do
      {:ok, file_entry} ->
        {:noreply, add_file_entry(socket, file_entry)}

      {:error, message} ->
        {:noreply, assign(socket, error_message: Livebook.Utils.upcase_first(message))}
    end
  end

  defp add_file_entry(socket, file_entry) do
    Livebook.Session.add_file_entries(socket.assigns.session.pid, [file_entry])
    push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}")
  end

  defp regular?(file, file_info) do
    file_info.exists and not FileSystem.File.dir?(file)
  end
end

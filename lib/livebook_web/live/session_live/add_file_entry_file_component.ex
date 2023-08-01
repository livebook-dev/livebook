defmodule LivebookWeb.SessionLive.AddFileEntryFileComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  alias Livebook.FileSystem

  @impl true
  def mount(socket) do
    {:ok,
     assign(socket,
       file: Livebook.Config.local_file_system_home(),
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
    changeset = changeset(Map.put(socket.assigns.changeset.params, "name", name))
    {:ok, assign(socket, file: file, file_info: file_info, changeset: changeset)}
  end

  def update(%{file_entry_result: file_entry_result}, socket) do
    socket = assign(socket, fetching: false)

    case file_entry_result do
      {:ok, file_entry} ->
        {:ok, add_file_entry(socket, file_entry)}

      {:error, message} ->
        {:ok, assign(socket, error_message: Livebook.Utils.upcase_first(message))}
    end
  end

  def update(assigns, socket) do
    {:ok, assign(socket, assigns)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col">
      <div :if={@error_message} class="mb-6 error-box">
        <%= @error_message %>
      </div>
      <div class="h-80" role="region" aria-label="file system">
        <.live_component
          module={LivebookWeb.FileSelectComponent}
          id="add-file-entry-select"
          file={@file}
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
              {"true", "Copy file contents to the notebook files directory"}
            ]}
          />
        </div>
        <div class="mt-6 flex space-x-3">
          <button
            class="button-base button-blue"
            type="submit"
            disabled={not @changeset.valid? or not regular?(@file, @file_info) or @fetching}
          >
            <.spinner :if={@fetching} class="mr-2" />
            <span>Add</span>
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
          async_create_attachment_file_entry(socket, file_entry)
          {:noreply, assign(socket, fetching: true, error_message: nil)}
        else
          {:noreply, add_file_entry(socket, file_entry)}
        end

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  defp async_create_attachment_file_entry(socket, file_entry) do
    pid = self()
    id = socket.assigns.id
    session = socket.assigns.session

    Task.Supervisor.async_nolink(Livebook.TaskSupervisor, fn ->
      file_entry_result = Livebook.Session.to_attachment_file_entry(session, file_entry)
      send_update(pid, __MODULE__, id: id, file_entry_result: file_entry_result)
    end)
  end

  defp add_file_entry(socket, file_entry) do
    Livebook.Session.add_file_entries(socket.assigns.session.pid, [file_entry])
    # We can't do push_patch from update/2, so we ask the LV to do so
    send(self(), {:push_patch, ~p"/sessions/#{socket.assigns.session.id}"})
    socket
  end

  defp regular?(file, file_info) do
    file_info.exists and not FileSystem.File.dir?(file)
  end
end

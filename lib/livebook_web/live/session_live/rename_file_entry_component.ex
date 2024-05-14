defmodule LivebookWeb.SessionLive.RenameFileEntryComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  @impl true
  def update(assigns, socket) do
    socket =
      socket
      |> assign(assigns)
      |> assign_new(:changeset, fn -> changeset(assigns.file_entry) end)

    {:ok, socket}
  end

  defp changeset(file_entry, attrs \\ %{}) do
    data = %{name: file_entry.name}
    types = %{name: :string}

    cast({data, types}, attrs, [:name])
    |> validate_required([:name])
    |> Livebook.Notebook.validate_file_entry_name(:name)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-4">
      <h3 class="text-2xl font-semibold text-gray-800">
        Rename file
      </h3>
      <.form
        :let={f}
        for={@changeset}
        as={:data}
        id="rename-file-entry-form"
        phx-change="validate"
        phx-submit="submit"
        phx-target={@myself}
      >
        <.text_field
          field={f[:name]}
          label="Name"
          id="add-file-entry-form-name"
          autocomplete="off"
          phx-debounce="200"
          autofocus
        />
        <div class="mt-6 flex space-x-3">
          <.button type="submit" disabled={not @changeset.valid?}>
            <.spinner class="hidden phx-submit-loading:block mr-2" />
            <span>Rename</span>
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
    changeset =
      socket.assigns.file_entry
      |> changeset(data)
      |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("submit", %{"data" => data}, socket) do
    %{file_entry: file_entry} = socket.assigns

    file_entry
    |> changeset(data)
    |> apply_action(:insert)
    |> case do
      {:ok, data} ->
        Livebook.Session.rename_file_entry(socket.assigns.session.pid, file_entry.name, data.name)
        {:noreply, push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}")}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end
end

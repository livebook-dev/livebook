defmodule LivebookWeb.SessionLive.AddFileEntryUnlistedComponent do
  use LivebookWeb, :live_component

  alias Livebook.FileSystem

  @impl true
  def mount(socket) do
    {:ok, assign(socket, selected_indices: [])}
  end

  @impl true
  def update(assigns, socket) do
    socket =
      socket
      |> assign(assigns)
      |> assign_new(:files, fn ->
        case FileSystem.File.list(assigns.session.files_dir) do
          {:ok, files} -> unlisted_files(files, assigns.file_entries)
          {:error, _} -> []
        end
      end)

    {:ok, socket}
  end

  defp unlisted_files(files, file_entries) do
    listed_names =
      for entry <- file_entries, entry.type == :attachment, into: MapSet.new(), do: entry.name

    files
    |> Enum.reject(fn file ->
      FileSystem.File.dir?(file) or FileSystem.File.name(file) in listed_names
    end)
    |> Enum.sort_by(&FileSystem.File.name/1)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col">
      <%= if @files == [] do %>
        <p class="text-gray-700">
          No other files found in the <code>files/</code> directory.
        </p>
      <% else %>
        <p class="text-gray-700">
          Here are other files from the <code>files/</code> directory that you may want to add.
        </p>
        <form
          phx-change="validate"
          phx-submit="add"
          phx-target={@myself}
          id="add-file-entry-form"
          class="mt-4"
        >
          <div class="flex flex-col gap-2">
            <.checkbox_field
              :for={{file, idx} <- Enum.with_index(@files)}
              id={"selected-indices-#{idx}"}
              name="selected_indices[]"
              checked_value={Integer.to_string(idx)}
              unchecked_value={nil}
              value={if(idx in @selected_indices, do: idx)}
              label={FileSystem.File.name(file)}
            />
          </div>
          <div class="mt-6 flex space-x-3">
            <button
              class="button-base button-blue"
              type="submit"
              disabled={Enum.empty?(@selected_indices)}
            >
              Add
            </button>
            <.link patch={~p"/sessions/#{@session.id}"} class="button-base button-outlined-gray">
              Cancel
            </.link>
          </div>
        </form>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_event("validate", params, socket) do
    selected_indices = selected_indices_from_params(params)
    {:noreply, assign(socket, selected_indices: selected_indices)}
  end

  def handle_event("add", params, socket) do
    selected_indices = selected_indices_from_params(params)

    file_entries =
      for {file, idx} <- Enum.with_index(socket.assigns.files),
          idx in selected_indices do
        name = FileSystem.File.name(file)
        %{name: name, type: :attachment}
      end

    Livebook.Session.add_file_entries(socket.assigns.session.pid, file_entries)

    {:noreply, push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}")}
  end

  defp selected_indices_from_params(params) do
    for idx <- params["selected_indices"] || [], into: MapSet.new(), do: String.to_integer(idx)
  end
end

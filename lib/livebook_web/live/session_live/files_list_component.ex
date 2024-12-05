defmodule LivebookWeb.SessionLive.FilesListComponent do
  use LivebookWeb, :live_component

  alias Livebook.FileSystem

  @impl true
  def mount(socket) do
    {:ok, assign(socket, transferring_file_entry_names: MapSet.new())}
  end

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    {attachment_file_entries, reference_file_entries} =
      Enum.split_with(assigns.file_entries, &(&1.type == :attachment))

    {:ok,
     assign(socket,
       attachment_file_entries: attachment_file_entries,
       reference_file_entries: reference_file_entries
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col grow" data-el-files-list>
      <h3 class="uppercase text-sm font-semibold text-gray-500">
        Files
      </h3>
      <div
        class="mt-5 h-20 rounded-lg border-2 border-dashed border-gray-400 flex items-center justify-center"
        data-el-files-drop-area
        id="files-dropzone"
        phx-hook="Dropzone"
      >
        <span class="font-medium text-gray-400">
          Add file
        </span>
      </div>
      <div class="flex mt-5" data-el-files-add-button>
        <.link
          class="w-full flex items-center justify-center p-8 py-1 space-x-2 text-sm font-medium text-gray-500 border border-gray-400 border-dashed rounded-xl hover:bg-gray-100"
          role="button"
          patch={~p"/sessions/#{@session.id}/add-file/storage"}
        >
          <.remix_icon icon="add-line" class="text-lg align-center" />
          <span>Add file</span>
        </.link>
      </div>
      <div class="mt-5 flex flex-col">
        <div class="flex justify-between items-center">
          <h3 class="text-sm font-semibold text-gray-700">
            References
          </h3>
          <.references_info_icon />
        </div>
        <.file_entries
          file_entries={@reference_file_entries}
          quarantine_file_entry_names={@quarantine_file_entry_names}
          transferring_file_entry_names={@transferring_file_entry_names}
          session={@session}
          myself={@myself}
        />
      </div>
      <div class="mt-5 flex flex-col">
        <div class="flex justify-between items-center">
          <h3 class="text-sm font-semibold text-gray-700">
            Attachments
          </h3>
          <.attachments_info_icon />
        </div>
        <.file_entries
          file_entries={@attachment_file_entries}
          quarantine_file_entry_names={@quarantine_file_entry_names}
          transferring_file_entry_names={@transferring_file_entry_names}
          session={@session}
          myself={@myself}
        />
      </div>
    </div>
    """
  end

  defp file_entries(%{file_entries: []} = assigns) do
    ~H"""
    <div class="mt-2 flex text-sm text-gray-500">
      No files yet
    </div>
    """
  end

  defp file_entries(assigns) do
    ~H"""
    <div class="mt-2 flex flex-col gap-1">
      <div
        :for={{file_entry, idx} <- Enum.with_index(@file_entries)}
        class="flex items-center justify-between"
      >
        <%= if file_entry.name in @quarantine_file_entry_names do %>
          <button
            class="flex items-center text-yellow-bright-500 cursor-pointer tooltip top"
            data-tooltip="Click to review access"
            phx-click={JS.push("review_file_entry_access", value: %{name: file_entry.name})}
          >
            <.remix_icon icon="alert-line" class="text-lg align-middle mr-2" />
            <span class="break-all">{file_entry.name}</span>
          </button>
        <% else %>
          <div
            class="flex items-center text-gray-500 cursor-grab"
            draggable="true"
            data-el-file-entry
            data-name={file_entry.name}
          >
            <.remix_icon
              icon={file_entry_icon(file_entry, @session)}
              class="text-lg align-middle mr-2"
            />
            <span class="break-all">{file_entry.name}</span>
          </div>
        <% end %>
        <%= if file_entry.name in @transferring_file_entry_names do %>
          <.spinner class="mr-[3px]" />
        <% else %>
          <.menu id={"file-entry-#{file_entry.type}-#{idx}-menu"} position="bottom-right">
            <:toggle>
              <.icon_button small aria-label="menu">
                <.remix_icon icon="more-2-line" />
              </.icon_button>
            </:toggle>
            <.menu_item>
              <button
                :if={file_entry.type != :attachment}
                role="menuitem"
                phx-click={
                  JS.push("transfer_file_entry", value: %{name: file_entry.name}, target: @myself)
                }
              >
                <.remix_icon icon="file-transfer-line" />
                <span>Move to attachments</span>
              </button>
            </.menu_item>
            <.menu_item>
              <a
                role="menuitem"
                href={~p"/sessions/#{@session.id}/download/files/#{file_entry.name}"}
                download
              >
                <.remix_icon icon="download-2-line" />
                <span>Download</span>
              </a>
            </.menu_item>
            <.menu_item>
              <button
                role="menuitem"
                phx-click={
                  JS.dispatch("lb:clipcopy",
                    detail: %{content: file_entry_location(file_entry, @session)}
                  )
                }
              >
                <.remix_icon icon="clipboard-line" />
                <span>Copy location</span>
              </button>
            </.menu_item>
            <.menu_item disabled={not Livebook.Session.file_entry_cacheable?(@session, file_entry)}>
              <button
                role="menuitem"
                phx-click={
                  JS.push("clear_file_entry_cache",
                    value: %{name: file_entry.name},
                    target: @myself
                  )
                }
              >
                <.remix_icon icon="eraser-line" />
                <span>Clear cache</span>
              </button>
            </.menu_item>
            <.menu_item>
              <.link
                role="menuitem"
                patch={~p"/sessions/#{@session.id}/rename-file/#{file_entry.name}"}
              >
                <.remix_icon icon="edit-line" />
                <span>Rename</span>
              </.link>
            </.menu_item>
            <.menu_item variant="danger">
              <button
                role="menuitem"
                phx-click={
                  JS.push("delete_file_entry", value: %{name: file_entry.name}, target: @myself)
                }
              >
                <.remix_icon icon="delete-bin-line" />
                <span>Delete</span>
              </button>
            </.menu_item>
          </.menu>
        <% end %>
      </div>
    </div>
    """
  end

  defp references_info_icon(assigns) do
    ~H"""
    <span
      class="tooltip bottom-left"
      data-tooltip={
        ~S'''
        References are files that point to
        existing resources on your disk, a
        remote storage, or a URL.
        '''
      }
    >
      <.icon_button small>
        <.remix_icon icon="question-line" />
      </.icon_button>
    </span>
    """
  end

  defp attachments_info_icon(assigns) do
    ~H"""
    <span
      class="tooltip bottom-left"
      data-tooltip={
        ~S'''
        Attachments are files stored in the
        files/ directory, kept and managed
        alongside your notebook.
        '''
      }
    >
      <.icon_button small>
        <.remix_icon icon="question-line" />
      </.icon_button>
    </span>
    """
  end

  defp file_entry_icon(%{type: :url}, _session), do: "links-line"

  defp file_entry_icon(%{type: :attachment}, session) do
    if FileSystem.File.local?(session.files_dir), do: "file-3-line", else: "cloud-line"
  end

  defp file_entry_icon(%{type: :file, file: file}, _session) do
    if FileSystem.File.local?(file), do: "file-3-line", else: "cloud-line"
  end

  defp file_entry_location(%{type: :url, url: url}, _session), do: url

  defp file_entry_location(%{type: :attachment, name: name}, session) do
    file = FileSystem.File.resolve(session.files_dir, name)
    file.path
  end

  defp file_entry_location(%{type: :file, file: file}, _session) do
    file.path
  end

  @impl true
  def handle_event("delete_file_entry", %{"name" => name}, socket) do
    if file_entry = find_file_entry(socket, name) do
      session = socket.assigns.session

      on_confirm = fn socket, options ->
        Livebook.Session.delete_file_entry(session.pid, file_entry.name)

        if Map.get(options, "delete_from_file_system", false) do
          file = FileSystem.File.resolve(session.files_dir, file_entry.name)

          case FileSystem.File.remove(file) do
            :ok ->
              socket

            {:error, error} ->
              put_flash(socket, :error, "Failed to remove #{file.path}, reason: #{error}")
          end
        else
          socket
        end
      end

      assigns = %{name: file_entry.name}

      description = ~H"""
      Are you sure you want to delete this file - <span class="font-semibold">“{@name}”</span>?
      """

      {:noreply,
       confirm(socket, on_confirm,
         title: "Delete file",
         description: description,
         confirm_text: "Delete",
         confirm_icon: "delete-bin-6-line",
         options:
           if file_entry.type == :attachment do
             [
               %{
                 name: "delete_from_file_system",
                 label: "Delete the corresponding file from its storage",
                 default: true,
                 disabled: false
               }
             ]
           else
             []
           end
       )}
    else
      {:noreply, socket}
    end
  end

  def handle_event("transfer_file_entry", %{"name" => name}, socket) do
    socket =
      if file_entry = find_file_entry(socket, name) do
        session = socket.assigns.session

        socket
        |> start_async(:transfer_file_entry, fn ->
          file_entry_result = Livebook.Session.to_attachment_file_entry(session, file_entry)
          {name, file_entry_result}
        end)
        |> update(:transferring_file_entry_names, &MapSet.put(&1, name))
      else
        socket
      end

    {:noreply, socket}
  end

  def handle_event("clear_file_entry_cache", %{"name" => name}, socket) do
    if file_entry = find_file_entry(socket, name) do
      Livebook.Session.clear_file_entry_cache(socket.assigns.session.id, file_entry.name)
    end

    {:noreply, socket}
  end

  @impl true
  def handle_async(:transfer_file_entry, {:ok, {name, file_entry_result}}, socket) do
    case file_entry_result do
      {:ok, file_entry} ->
        Livebook.Session.add_file_entries(socket.assigns.session.pid, [file_entry])

      {:error, message} ->
        send(self(), {:put_flash, :error, Livebook.Utils.upcase_first(message)})
    end

    socket = update(socket, :transferring_file_entry_names, &MapSet.delete(&1, name))

    {:noreply, socket}
  end

  defp find_file_entry(socket, name) do
    Enum.find(socket.assigns.file_entries, &(&1.name == name))
  end
end

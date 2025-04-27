defmodule LivebookWeb.FileSelectComponent do
  use LivebookWeb, :live_component

  # The component expects:
  #
  #   * `:file` - the currently entered file
  #
  #   * `:hub` - the hub to show file systems from. By default file
  #     systems from all hubs are available
  #
  #   * `:running_files` - the list of notebook files that are already
  #     linked to running sessions
  #
  #   * `:extnames` - a list of file extensions that should be shown
  #
  #   * `:on_submit` - `%JS{}` to execute on form submission
  #
  #   * `:target` - either a pid or `{component_module, id}` to send
  #     events to
  #
  # The target receives `{:set_file, file, %{exists: boolean()}}` event
  # whenever the file changes.
  #
  # Optionally inner block may be passed (e.g. with action buttons)
  # and it's rendered next to the text input.
  #
  # To force the component to refetch the displayed files you can
  # `send_update` with `force_reload: true` to the component.

  import LivebookWeb.FileSystemComponents

  alias Livebook.FileSystem

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign_new(:inner_block, fn -> nil end)
     |> assign(
       # Component default attribute values
       inner_block: nil,
       file_system_select_disabled: false,
       on_submit: nil,
       # State
       current_dir: nil,
       deleting_file: nil,
       renaming_file: nil,
       renamed_name: nil,
       error_message: nil,
       configure_path: nil,
       file_systems: []
     )
     |> allow_upload(:folder,
       accept: :any,
       auto_upload: true,
       max_entries: 1,
       progress: &handle_progress/3,
       writer: fn _name, entry, socket ->
         file = FileSystem.File.resolve(socket.assigns.current_dir, entry.client_name)
         {LivebookWeb.FileSystemWriter, [file: file]}
       end
     )}
  end

  @impl true
  def update(assigns, socket) do
    {force_reload?, assigns} = Map.pop(assigns, :force_reload, false)

    running_files_changed? = assigns.running_files != (socket.assigns[:running_files] || [])

    socket =
      socket
      |> assign(assigns)
      |> update_file_infos(force_reload? or running_files_changed?)

    {file_systems, configure_hub_id} =
      if hub = socket.assigns[:hub],
        do: {Livebook.Hubs.get_file_systems(hub), hub.id},
        else: {Livebook.Hubs.get_file_systems(), Livebook.Hubs.Personal.id()}

    configure_path = ~p"/hub/#{configure_hub_id}/file-systems/new"

    {:ok, assign(socket, file_systems: file_systems, configure_path: configure_path)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="h-full flex flex-col">
      <h2 class="sr-only">File system</h2>
      <div class="flex space-x-3 items-center mb-4">
        <div class="grow flex space-x-1">
          <.file_system_menu_button
            id={"#{@id}-file-system-menu"}
            file={@file}
            file_systems={@file_systems}
            configure_path={@configure_path}
            file_system_select_disabled={@file_system_select_disabled}
            myself={@myself}
          />
          <form
            id={"#{@id}-path-form"}
            class="grow"
            phx-change={JS.push("set_path", target: @myself)}
            phx-submit={@on_submit}
            phx-nosubmit={@on_submit == nil}
          >
            <.text_field
              id={"#{@id}-path-input"}
              aria-label="file path"
              phx-hook="FocusOnUpdate"
              name="path"
              placeholder="File"
              value={@file.path}
              spellcheck="false"
              autocomplete="off"
            />
          </form>
        </div>
        <.menu
          id={"#{@id}-new-item-menu"}
          disabled={@file_system_select_disabled}
          position="bottom-right"
        >
          <:toggle>
            <.icon_button tabindex="-1" aria-label="add">
              <.remix_icon icon="add-line" />
            </.icon_button>
          </:toggle>
          <.menu_item>
            <button role="menuitem" phx-click={js_show_new_item_section("#{@id}-new-dir-section")}>
              <.remix_icon icon="folder-add-line" />
              <span>New directory</span>
            </button>
          </.menu_item>
          <.menu_item>
            <button
              role="menuitem"
              phx-click={js_show_new_item_section("#{@id}-new-notebook-section")}
            >
              <.remix_icon icon="file-add-line" />
              <span>New notebook</span>
            </button>
          </.menu_item>
          <.menu_item>
            <button role="menuitem" phx-click="set_default_directory" phx-target={@myself}>
              <.remix_icon icon="home-6-line" />
              <span>Set as default directory</span>
            </button>
          </.menu_item>
        </.menu>
        <div :if={@inner_block}>
          {render_slot(@inner_block)}
        </div>
      </div>
      <div class="flex flex-col space-y-2">
        <div :if={@error_message} class="error-box flex justify-between items-center">
          <span>{@error_message}</span>
          <button phx-click="clear_error" phx-target={@myself}>
            <.remix_icon icon="delete-bin-6-line" class="text-lg align-middle" />
          </button>
        </div>
        <div
          :if={@deleting_file}
          class="mb-4 px-4 py-3 flex space-x-4 items-center border border-gray-200 rounded-lg"
        >
          <p class="grow text-gray-700 text-sm">
            Are you sure you want to irreversibly delete <span class="font-semibold">{@deleting_file.path}</span>?
          </p>
          <div class="flex space-x-4">
            <button
              class="text-red-600 font-medium text-sm whitespace-nowrap"
              phx-click="do_delete_file"
              phx-target={@myself}
            >
              <.remix_icon icon="delete-bin-6-line" class="align-middle mr-1" /> Delete
            </button>
            <button
              class="text-gray-600 font-medium text-sm"
              phx-click="cancel_delete_file"
              phx-target={@myself}
            >
              Cancel
            </button>
          </div>
        </div>
      </div>

      <.new_item_section
        id={"#{@id}-new-dir-section"}
        type="dir"
        icon="folder-add-fill"
        myself={@myself}
      />
      <.new_item_section
        id={"#{@id}-new-notebook-section"}
        type="notebook"
        icon="file-add-line"
        myself={@myself}
      />

      <div
        class="grow -m-1 p-1 h-full rounded-lg overflow-y-auto tiny-scrollbar"
        tabindex="-1"
        phx-hook="Dropzone"
        id={"#{@id}-file-select-upload-dropzone"}
      >
        <form phx-change="file_validate" phx-drop-target={@uploads.folder.ref} phx-target={@myself}>
          <.live_file_input
            upload={@uploads.folder}
            class="hidden"
            aria-labelledby="import-from-file"
          />

          <div
            :if={@uploads.folder.entries != []}
            class="border-b border-dashed border-grey-200 mb-2 pb-2"
          >
            <div :for={file <- @uploads.folder.entries} class="p-2 flex gap-2 items-center">
              <.spinner />
              <span class="font-medium text-gray-500">{file.client_name}</span>
              <div class="grow" />
              <.icon_button type="button" phx-click="clear-file" phx-target={@myself} tabindex="-1">
                <.remix_icon icon="close-line" />
              </.icon_button>
            </div>
          </div>

          <div
            :if={@highlighted_file_infos != []}
            class="grid grid-cols-2 lg:grid-cols-3 gap-2 border-b border-dashed border-grey-200 mb-2 pb-2"
          >
            <%= for file_info <- Enum.take(@highlighted_file_infos, visible_files_limit()) do %>
              <.file
                id={"#{@id}-file-#{file_info.id}"}
                file_info={file_info}
                myself={@myself}
                renaming_file={@renaming_file}
                renamed_name={@renamed_name}
              />
            <% end %>
            <.more_files_indicator length={length(@highlighted_file_infos)} />
          </div>

          <div class="grid grid-cols-2 lg:grid-cols-3 gap-2">
            <%= for file_info <- Enum.take(@unhighlighted_file_infos, visible_files_limit()) do %>
              <.file
                id={"#{@id}-file-#{file_info.id}"}
                file_info={file_info}
                myself={@myself}
                renaming_file={@renaming_file}
                renamed_name={@renamed_name}
              />
            <% end %>
            <.more_files_indicator length={length(@unhighlighted_file_infos)} />
          </div>
        </form>
      </div>
    </div>
    """
  end

  defp new_item_section(assigns) do
    ~H"""
    <div
      class="hidden grid grid-cols-2 lg:grid-cols-3 gap-2 border-b border-dashed border-grey-200 mb-2 pb-2"
      id={@id}
    >
      <form
        class="flex space-x-2 items-center p-2 rounded-lg"
        phx-submit={JS.push("create_#{@type}", target: @myself) |> js_hide_new_item_section(@id)}
      >
        <span class="block">
          <.remix_icon icon={@icon} class="text-xl align-middle text-gray-400" />
        </span>
        <span class="flex font-medium text-gray-500">
          <div phx-window-keydown={js_hide_new_item_section(@id)} phx-key="escape">
            <input
              type="text"
              name="name"
              aria-label="new directory"
              spellcheck="false"
              autocomplete="off"
              phx-blur={js_hide_new_item_section(@id)}
            />
          </div>
        </span>
      </form>
    </div>
    """
  end

  defp file_system_menu_button(assigns) do
    ~H"""
    <.menu id={@id} disabled={@file_system_select_disabled} position="bottom-left">
      <:toggle>
        <.button
          color="gray"
          type="button"
          class="pl-3 pr-2"
          aria-label="switch file storage"
          disabled={@file_system_select_disabled}
        >
          <span>{file_system_name(@file.file_system_module)}</span>
          <div class="pl-0.5 flex items-center">
            <.remix_icon icon="arrow-down-s-line" class="text-lg leading-none" />
          </div>
        </.button>
      </:toggle>
      <%= for file_system <- @file_systems do %>
        <%= if file_system.id == @file.file_system_id do %>
          <.menu_item variant="selected">
            <button id={"#{@id}-file-system-#{file_system.id}"} role="menuitem">
              <.file_system_icon file_system={file_system} />
              <span>{file_system_label(file_system)}</span>
            </button>
          </.menu_item>
        <% else %>
          <.menu_item>
            <button
              id={"#{@id}-file-system-#{file_system.id}"}
              role="menuitem"
              phx-target={@myself}
              phx-click="set_file_system"
              phx-value-id={file_system.id}
            >
              <.file_system_icon file_system={file_system} />
              <span>{file_system_label(file_system)}</span>
            </button>
          </.menu_item>
        <% end %>
      <% end %>
      <.menu_item>
        <.link navigate={@configure_path} class="border-t border-gray-200" role="menuitem">
          <.remix_icon icon="settings-3-line" />
          <span>Configure</span>
        </.link>
      </.menu_item>
    </.menu>
    """
  end

  defp file(%{file_info: %{file: file}, renaming_file: file} = assigns) do
    ~H"""
    <div class="flex space-x-2 items-center p-2 rounded-lg">
      <span class="block">
        <.remix_icon icon="edit-line" class="text-xl align-middle text-gray-400" />
      </span>
      <span class="flex font-medium text-gray-500">
        <div phx-window-keydown="cancel_rename_file" phx-key="escape" phx-target={@myself}>
          <input
            class="w-full"
            type="text"
            value={@renamed_name}
            autofocus
            spellcheck="false"
            autocomplete="off"
            phx-blur="cancel_rename_file"
            phx-window-keydown="do_rename_file"
            phx-key="enter"
            phx-target={@myself}
          />
        </div>
      </span>
    </div>
    """
  end

  defp file(assigns) do
    icon =
      case assigns.file_info do
        %{is_running: true} -> "play-circle-line"
        %{is_dir: true} -> "folder-fill"
        _ -> "file-code-line"
      end

    assigns = assign(assigns, :icon, icon)

    ~H"""
    <.menu id={@id} secondary_click>
      <:toggle>
        <button
          type="button"
          class="w-full flex space-x-2 items-center p-2 rounded-lg hover:bg-gray-100 focus:ring-1 focus:ring-gray-400 focus-visible:outline-none"
          data-toggle
          aria-label={"#{if @file_info.name == "..", do: "parent directory", else: @file_info.name}"}
          phx-click="set_path"
          phx-value-path={@file_info.file.path}
          phx-target={@myself}
        >
          <span class="block">
            <.remix_icon
              icon={@icon}
              class={
                "text-xl align-middle #{if(@file_info.is_running, do: "text-green-300", else: "text-gray-400")}"
              }
            />
          </span>
          <span class={
            "flex font-medium overflow-hidden whitespace-nowrap #{if(@file_info.is_running, do: "text-green-300", else: "text-gray-500")}"
          }>
            <span>
              {@file_info.unhighlighted_before}
            </span>
            <span
              :if={@file_info.highlighted != ""}
              class={[
                "font-medium",
                @file_info.unhighlighted_after == "" && "overflow-hidden text-ellipsis",
                if(@file_info.is_running, do: "text-green-400", else: "text-gray-900")
              ]}
            >
              {@file_info.highlighted}
            </span>
            <span class="overflow-hidden text-ellipsis">
              {@file_info.unhighlighted_after}
            </span>
          </span>
        </button>
      </:toggle>
      <.menu_item :if={@file_info.editable}>
        <button
          type="button"
          role="menuitem"
          aria-label="rename file"
          phx-click="rename_file"
          phx-target={@myself}
          phx-value-path={@file_info.file.path}
        >
          <.remix_icon icon="edit-line" />
          <span>Rename</span>
        </button>
      </.menu_item>
      <.menu_item :if={@file_info.editable} variant="danger">
        <button
          type="button"
          role="menuitem"
          aria-label="delete file"
          phx-click="delete_file"
          phx-target={@myself}
          phx-value-path={@file_info.file.path}
        >
          <.remix_icon icon="delete-bin-6-line" />
          <span>Delete</span>
        </button>
      </.menu_item>
    </.menu>
    """
  end

  defp more_files_indicator(assigns) do
    ~H"""
    <div
      :if={@length > visible_files_limit()}
      class="col-span-full text-sm text-medium text-gray-500 flex flex-col items-center gap-1"
    >
      <.remix_icon icon="more-line" class="text-lg" />
      {@length - visible_files_limit()} more files (search to see)
    </div>
    """
  end

  defp visible_files_limit(), do: 200

  defp js_show_new_item_section(js \\ %JS{}, id) do
    js
    |> JS.show(to: "##{id}")
    |> JS.dispatch("lb:set_value", to: "##{id} input", detail: %{value: ""})
    |> JS.dispatch("lb:focus", to: "##{id} input")
  end

  defp js_hide_new_item_section(js \\ %JS{}, id) do
    js
    |> JS.hide(to: "##{id}")
  end

  defp handle_progress(:folder, entry, socket) when entry.done? do
    :ok = consume_uploaded_entry(socket, entry, fn %{} -> {:ok, :ok} end)
    {:noreply, update_file_infos(socket, true)}
  end

  defp handle_progress(:folder, _entry, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("file_validate", _, socket) do
    {:noreply, socket}
  end

  def handle_event("set_file_system", %{"id" => file_system_id}, socket) do
    file_system = Enum.find(socket.assigns.file_systems, &(&1.id == file_system_id))

    file = FileSystem.File.new(file_system)

    send_event(socket.assigns.target, {:set_file, file, %{exists: true}})

    {:noreply, socket}
  end

  def handle_event("set_path", %{"path" => path}, socket) do
    file_system =
      Enum.find(socket.assigns.file_systems, &(&1.id == socket.assigns.file.file_system_id))

    file =
      file_system
      |> FileSystem.File.new()
      |> FileSystem.File.resolve(path)

    info =
      socket.assigns.file_infos
      |> Enum.find(&(&1.file.path == path))
      |> case do
        nil -> %{exists: false}
        _info -> %{exists: true}
      end

    send_event(socket.assigns.target, {:set_file, file, info})

    {:noreply, socket}
  end

  def handle_event("clear_error", %{}, socket) do
    {:noreply, put_error(socket, nil)}
  end

  def handle_event("create_dir", %{"name" => name}, socket) do
    socket =
      case create_dir(socket.assigns.current_dir, name) do
        :ok -> update_file_infos(socket, true)
        {:error, error} -> put_error(socket, error)
      end

    {:noreply, socket}
  end

  def handle_event("create_notebook", %{"name" => name}, socket) do
    socket =
      case create_notebook(socket.assigns.current_dir, name) do
        :ok -> update_file_infos(socket, true)
        {:error, error} -> put_error(socket, error)
      end

    {:noreply, socket}
  end

  def handle_event("delete_file", %{"path" => path}, socket) do
    %{file: file} = Enum.find(socket.assigns.file_infos, &(&1.file.path == path))
    {:noreply, assign(socket, deleting_file: file)}
  end

  def handle_event("cancel_delete_file", %{}, socket) do
    {:noreply, assign(socket, deleting_file: nil)}
  end

  def handle_event("do_delete_file", %{}, socket) do
    socket =
      case delete_file(socket.assigns.deleting_file) do
        :ok ->
          socket
          |> assign(deleting_file: nil)
          |> update_file_infos(true)

        {:error, error} ->
          put_error(socket, error)
      end

    {:noreply, socket}
  end

  def handle_event("rename_file", %{"path" => path}, socket) do
    file_info = Enum.find(socket.assigns.file_infos, &(&1.file.path == path))
    {:noreply, assign(socket, renaming_file: file_info.file, renamed_name: file_info.name)}
  end

  def handle_event("cancel_rename_file", %{}, socket) do
    {:noreply, assign(socket, renaming_file: nil)}
  end

  def handle_event("do_rename_file", %{"value" => name}, socket) do
    socket =
      if renaming_file = socket.assigns.renaming_file do
        case rename_file(renaming_file, name) do
          :ok ->
            socket
            |> assign(renaming_file: nil)
            |> update_file_infos(true)

          {:error, error} ->
            socket
            |> assign(renamed_name: name)
            |> put_error(error)
        end
      else
        socket
      end

    {:noreply, socket}
  end

  def handle_event("clear-file", %{}, socket) do
    {socket, _entries} = Phoenix.LiveView.Upload.maybe_cancel_uploads(socket)
    {:noreply, assign(socket, error: false)}
  end

  def handle_event("set_default_directory", _params, socket) do
    {dir, _prefix} = dir_and_prefix(socket.assigns.file)
    Livebook.Settings.set_default_dir(dir)
    {:noreply, socket}
  end

  defp update_file_infos(%{assigns: assigns} = socket, force_reload?) do
    current_file_infos = assigns[:file_infos] || []
    {dir, prefix} = dir_and_prefix(assigns.file)

    {file_infos, socket} =
      if dir != assigns.current_dir or force_reload? do
        case get_file_infos(dir, assigns.extnames, assigns.running_files) do
          {:ok, file_infos} ->
            {file_infos, assign(socket, :current_dir, dir)}

          {:error, error} ->
            {current_file_infos, put_error(socket, error)}
        end
      else
        {current_file_infos, socket}
      end

    file_infos = annotate_matching(file_infos, prefix)

    {unhighlighted_file_infos, highlighted_file_infos} =
      Enum.split_with(file_infos, &(&1.highlighted == ""))

    # List files with prefix matching first
    highlighted_file_infos =
      Enum.sort_by(highlighted_file_infos, &if(&1.unhighlighted_before == "", do: 1, else: 2))

    assign(socket,
      file_infos: file_infos,
      unhighlighted_file_infos: unhighlighted_file_infos,
      highlighted_file_infos: highlighted_file_infos
    )
  end

  defp annotate_matching(file_infos, prefix) do
    for %{name: name} = info <- file_infos do
      case String.split(name, prefix, parts: 2) do
        [unhighlighted_before, unhighlighted_after] ->
          %{
            info
            | unhighlighted_before: unhighlighted_before,
              highlighted: prefix,
              unhighlighted_after: unhighlighted_after
          }

        [^name] ->
          %{info | unhighlighted_before: "", highlighted: "", unhighlighted_after: name}
      end
    end
  end

  # Phrase after the last slash is used as a search prefix within
  # the given directory.
  #
  # Given "/foo/bar", we use "bar" to filter files within "/foo/".
  # Given "/foo/bar/", we use "" to filter files within "/foo/bar/".
  defp dir_and_prefix(file) do
    if FileSystem.File.dir?(file) do
      {file, ""}
    else
      {FileSystem.File.containing_dir(file), FileSystem.File.name(file)}
    end
  end

  defp get_file_infos(dir, extnames, running_files) do
    with {:ok, files} <- FileSystem.File.list(dir) do
      file_infos =
        files
        |> Enum.map(fn file ->
          name = FileSystem.File.name(file)
          file_info(file, name, running_files)
        end)
        |> Enum.filter(fn info ->
          not hidden?(info.name) and (info.is_dir or valid_extension?(info.name, extnames))
        end)
        |> Kernel.++(
          case FileSystem.File.containing_dir(dir) do
            ^dir -> []
            parent -> [file_info(parent, "..", running_files, editable: false)]
          end
        )
        |> Enum.sort_by(fn file -> {!file.is_dir, file.name} end)

      {:ok, file_infos}
    end
  end

  defp file_info(file, name, running_files, opts \\ []) do
    %{
      id: Base.url_encode64(file.path, padding: false),
      name: name,
      unhighlighted_before: "",
      highlighted: "",
      unhighlighted_after: name,
      file: file,
      is_dir: FileSystem.File.dir?(file),
      is_running: running?(file, running_files),
      editable: Keyword.get(opts, :editable, true)
    }
  end

  defp running?(file, running_files) do
    Enum.any?(running_files, &FileSystem.File.equal?(&1, file))
  end

  defp hidden?(filename) do
    String.starts_with?(filename, ".")
  end

  defp valid_extension?(_filename, :any), do: true

  defp valid_extension?(filename, extnames) do
    Path.extname(filename) in extnames
  end

  defp put_error(socket, nil) do
    assign(socket, :error_message, nil)
  end

  defp put_error(socket, :ignore) do
    socket
  end

  defp put_error(socket, message) when is_binary(message) do
    assign(socket, :error_message, Livebook.Utils.upcase_first(message))
  end

  defp create_dir(_parent_dir, ""), do: {:error, :ignore}

  defp create_dir(parent_dir, name) do
    new_dir = FileSystem.File.resolve(parent_dir, name <> "/")
    FileSystem.File.create_dir(new_dir)
  end

  defp create_notebook(_parent_dir, ""), do: {:error, :ignore}

  defp create_notebook(parent_dir, name) do
    {source, _warnings} =
      Livebook.Session.default_notebook() |> Livebook.LiveMarkdown.notebook_to_livemd()

    new_file =
      parent_dir
      |> FileSystem.File.resolve(name)
      |> FileSystem.File.ensure_extension(Livebook.LiveMarkdown.extension())

    with {:ok, exists?} <- FileSystem.File.exists?(new_file) do
      if exists? do
        {:error, :ignore}
      else
        FileSystem.File.write(new_file, source)
      end
    end
  end

  defp delete_file(file) do
    FileSystem.File.remove(file)
  end

  defp rename_file(_file, ""), do: {:error, :ignore}

  defp rename_file(file, name) do
    parent_dir = FileSystem.File.containing_dir(file)
    new_name = if FileSystem.File.dir?(file), do: name <> "/", else: name
    new_file = FileSystem.File.resolve(parent_dir, new_name)
    FileSystem.File.rename(file, new_file)
  end
end

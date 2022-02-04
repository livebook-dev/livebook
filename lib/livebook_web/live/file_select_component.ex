defmodule LivebookWeb.FileSelectComponent do
  use LivebookWeb, :live_component

  # The component expects:
  #
  #   * `file` - the currently entered file
  #
  #   * `running_files` - the list of notebook files that are already
  #     linked to running sessions
  #
  #   * `extnames` - a list of file extensions that should be shown
  #
  #   * `submit_event` - the process event sent on form submission,
  #     use `nil` for no action
  #
  # The parent live view receives a `{:set_file, file, %{exists: boolean()}}`
  # message whenever the file changes.
  #
  # Optionally inner block may be passed (e.g. with action buttons)
  # and it's rendered next to the text input.
  #
  # To force the component to refetch the displayed files you can
  # `send_update` with `force_reload: true` to the component.

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
       submit_event: nil,
       # State
       current_dir: nil,
       deleting_file: nil,
       renaming_file: nil,
       renamed_name: nil,
       error_message: nil,
       file_systems: Livebook.Settings.file_systems()
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

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="h-full flex flex-col">
      <h2 class="sr-only">File system</h2>
      <div class="flex space-x-3 items-center mb-4">
        <div class="grow flex space-x-1">
          <.file_system_menu_button
            file={@file}
            file_systems={@file_systems}
            file_system_select_disabled={@file_system_select_disabled}
            socket={@socket}
            myself={@myself} />
          <form class="grow"
            phx-change="set_path"
            phx-submit={if @submit_event, do: "submit"}
            onsubmit={unless @submit_event, do: "return false"}
            phx-target={@myself}>
            <input class="input"
              id="input-path"
              aria-label="file path"
              phx-hook="FocusOnUpdate"
              type="text"
              name="path"
              placeholder="File"
              value={@file.path}
              spellcheck="false"
              autocomplete="off" />
          </form>
        </div>
        <span class={"tooltip #{if(@inner_block, do: "top", else: "left")}"} data-tooltip="New directory">
          <button class="icon-button"
            tabindex="-1"
            aria-label="new directory"
            phx-click={js_show_new_dir_section()}>
            <.remix_icon icon="add-line" class="text-xl" />
          </button>
        </span>
        <%= if @inner_block do %>
          <div>
            <%= render_slot(@inner_block) %>
          </div>
        <% end %>
      </div>
      <div class="flex flex-col space-y-2">
        <%= if @error_message do %>
          <div class="error-box flex justify-between items-center">
            <span><%= @error_message %></span>
            <button phx-click="clear_error" phx-target={@myself}>
              <.remix_icon icon="delete-bin-6-line" class="text-lg align-middle" />
            </button>
          </div>
        <% end %>
        <%= if @deleting_file do %>
          <div class="mb-4 px-4 py-3 flex space-x-4 items-center border border-gray-200 rounded-lg">
            <p class="grow text-gray-700 text-sm">
              Are you sure you want to irreversibly delete
              <span class="font-semibold"><%= @deleting_file.path %></span>?
            </p>
            <div class="flex space-x-4">
              <button class="text-red-600 font-medium text-sm whitespace-nowrap"
                phx-click="do_delete_file"
                phx-target={@myself}>
                <.remix_icon icon="delete-bin-6-line" class="align-middle mr-1" />
                Delete
              </button>
              <button class="text-gray-600 font-medium text-sm"
                phx-click="cancel_delete_file"
                phx-target={@myself}>
                Cancel
              </button>
            </div>
          </div>
        <% end %>
      </div>
      <div class="grow -m-1 p-1 overflow-y-auto tiny-scrollbar" tabindex="-1">
        <div class="hidden grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-2 border-b border-dashed border-grey-200 mb-2 pb-2"
          id="new_dir_section">
          <div class="flex space-x-2 items-center p-2 rounded-lg">
            <span class="block">
              <.remix_icon icon="folder-add-fill" class="text-xl align-middle text-gray-400" />
            </span>
            <span class="flex font-medium text-gray-500">
              <div
                phx-window-keydown={js_hide_new_dir_section()}
                phx-key="escape"
                phx-target={@myself}>
                <input
                  id="new_dir_input"
                  aria-label="new directory"
                  type="text"
                  spellcheck="false"
                  autocomplete="off"
                  phx-blur={js_hide_new_dir_section()}
                  phx-window-keydown={JS.push("create_dir", target: @myself) |> js_hide_new_dir_section()}
                  phx-key="enter" />
              </div>
            </span>
          </div>
        </div>

        <%= if any_highlighted?(@file_infos) do %>
          <div class="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-2 border-b border-dashed border-grey-200 mb-2 pb-2">
            <%= for file_info <- @file_infos, file_info.highlighted != "" do %>
              <.file
                file_info={file_info}
                myself={@myself}
                renaming_file={@renaming_file}
                renamed_name={@renamed_name} />
            <% end %>
          </div>
        <% end %>

        <div class="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-2">
          <%= for file_info <- @file_infos, file_info.highlighted == "" do %>
            <.file
              file_info={file_info}
              myself={@myself}
              renaming_file={@renaming_file}
              renamed_name={@renamed_name} />
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  defp any_highlighted?(file_infos) do
    Enum.any?(file_infos, &(&1.highlighted != ""))
  end

  defp file_system_menu_button(assigns) do
    ~H"""
    <.menu id="file-system-menu" disabled={@file_system_select_disabled} position="left">
      <:toggle>
        <button type="button" class="button-base button-gray button-square-icon"
          aria-label="switch file system"
          disabled={@file_system_select_disabled}>
          <.file_system_icon file_system={@file.file_system} />
        </button>
      </:toggle>
      <:content>
        <%= for {file_system, index} <- @file_systems |> Enum.with_index() do %>
          <%= if file_system == @file.file_system do %>
            <button class="menu-item text-gray-900" role="menuitem">
              <.file_system_icon file_system={file_system} />
              <span class="font-medium"><%= file_system_label(file_system) %></span>
            </button>
          <% else %>
            <button class="menu-item text-gray-500"
              role="menuitem"
              phx-target={@myself}
              phx-click="set_file_system"
              phx-value-index={index}>
              <.file_system_icon file_system={file_system} />
              <span class="font-medium"><%= file_system_label(file_system) %></span>
            </button>
          <% end %>
        <% end %>
        <%= live_patch to: Routes.settings_path(@socket, :page),
              class: "menu-item text-gray-500 border-t border-gray-200",
              role: "menuitem" do %>
          <.remix_icon icon="settings-3-line" />
          <span class="font-medium">Configure</span>
        <% end %>
      </:content>
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
        <div
          phx-window-keydown="cancel_rename_file"
          phx-key="escape"
          phx-target={@myself}>
          <input class="w-full"
            type="text"
            value={@renamed_name}
            autofocus
            spellcheck="false"
            autocomplete="off"
            phx-blur="cancel_rename_file"
            phx-window-keydown="do_rename_file"
            phx-key="enter"
            phx-target={@myself} />
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
    <.menu id={"file-#{Base.encode16 @file_info.file.path}"} secondary_click>
      <:toggle>
        <button class="w-full flex space-x-2 items-center p-2 rounded-lg hover:bg-gray-100 focus:ring-1 focus:ring-gray-400"
          data-toggle
          aria-label={"#{if @file_info.name == "..", do: "parent directory", else: @file_info.name}"}
          phx-click="set_path"
          phx-value-path={@file_info.file.path}
          phx-target={@myself}>
          <span class="block">
            <.remix_icon icon={@icon} class={"text-xl align-middle #{if(@file_info.is_running, do: "text-green-300", else: "text-gray-400")}"} />
          </span>
          <span class={"flex font-medium overflow-hidden whitespace-nowrap #{if(@file_info.is_running, do: "text-green-300", else: "text-gray-500")}"}>
            <%= if @file_info.highlighted != "" do %>
              <span class={
                "font-medium
                  #{if(@file_info.unhighlighted == "", do: "overflow-hidden text-ellipsis")}
                  #{if(@file_info.is_running, do: "text-green-400", else: "text-gray-900")}"}>
                <%= @file_info.highlighted %>
              </span>
            <% end %>
            <span class="overflow-hidden text-ellipsis">
              <%= @file_info.unhighlighted %>
            </span>
          </span>
        </button>
      </:toggle>
      <:content>
        <%= if @file_info.editable do %>
          <button class="menu-item text-gray-500"
            role="menuitem"
            aria-label="rename file"
            phx-click="rename_file"
            phx-target={@myself}
            phx-value-path={@file_info.file.path}>
            <.remix_icon icon="edit-line" />
            <span class="font-medium">Rename</span>
          </button>
          <button class="menu-item text-red-600"
            role="menuitem"
            aria-label="delete file"
            phx-click="delete_file"
            phx-target={@myself}
            phx-value-path={@file_info.file.path}>
            <.remix_icon icon="delete-bin-6-line" />
            <span class="font-medium">Delete</span>
          </button>
        <% end %>
      </:content>
    </.menu>
    """
  end

  defp js_show_new_dir_section(js \\ %JS{}) do
    js
    |> JS.show(to: "#new_dir_section")
    |> JS.dispatch("lb:set_value", to: "#new_dir_input", detail: %{value: ""})
    |> JS.dispatch("lb:focus", to: "#new_dir_input")
  end

  defp js_hide_new_dir_section(js \\ %JS{}) do
    js
    |> JS.hide(to: "#new_dir_section")
  end

  @impl true
  def handle_event("set_file_system", %{"index" => index}, socket) do
    index = String.to_integer(index)
    file_system = Enum.at(socket.assigns.file_systems, index)
    file = FileSystem.File.new(file_system)

    send(self(), {:set_file, file, %{exists: true}})

    {:noreply, socket}
  end

  def handle_event("set_path", %{"path" => path}, socket) do
    file = FileSystem.File.new(socket.assigns.file.file_system) |> FileSystem.File.resolve(path)

    info =
      socket.assigns.file_infos
      |> Enum.find(&(&1.file.path == path))
      |> case do
        nil -> %{exists: false}
        _info -> %{exists: true}
      end

    send(self(), {:set_file, file, info})

    {:noreply, socket}
  end

  def handle_event("submit", %{}, socket) do
    if submit_event = socket.assigns.submit_event do
      send(self(), submit_event)
    end

    {:noreply, socket}
  end

  def handle_event("clear_error", %{}, socket) do
    {:noreply, put_error(socket, nil)}
  end

  def handle_event("create_dir", %{"value" => name}, socket) do
    socket =
      case create_dir(socket.assigns.current_dir, name) do
        :ok ->
          socket
          |> update_file_infos(true)

        {:error, error} ->
          socket
          |> put_error(error)
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

    assign(socket, :file_infos, annotate_matching(file_infos, prefix))
  end

  defp annotate_matching(file_infos, prefix) do
    for %{name: name} = info <- file_infos do
      if String.starts_with?(name, prefix) do
        %{info | highlighted: prefix, unhighlighted: String.replace_prefix(name, prefix, "")}
      else
        %{info | highlighted: "", unhighlighted: name}
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
      name: name,
      highlighted: "",
      unhighlighted: name,
      file: file,
      is_dir: FileSystem.File.dir?(file),
      is_running: file in running_files,
      editable: Keyword.get(opts, :editable, true)
    }
  end

  defp hidden?(filename) do
    String.starts_with?(filename, ".")
  end

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

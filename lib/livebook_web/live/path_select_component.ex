defmodule LivebookWeb.PathSelectComponent do
  use LivebookWeb, :live_component

  # The component expects:
  #
  # * `path` - the currently entered path
  # * `running_paths` - the list of notebook paths that are already linked to running sessions
  # * `extnames` - a list of file extensions that should be shown
  # * `phx_target` - id of the component to send update events to or nil to send to the parent LV
  # * `phx_submit` - the event name sent on form submission, use `nil` for no action
  #
  # The target receives `set_path` events with `%{"path" => path}` payload.
  #
  # Optionally inner block may be passed (e.g. with action buttons)
  # and it's rendered next to the text input.

  @impl true
  def mount(socket) do
    inner_block = Map.get(socket.assigns, :inner_block, nil)
    {:ok, assign(socket, inner_block: inner_block, current_dir: nil)}
  end

  @impl true
  def update(assigns, socket) do
    %{assigns: assigns} = socket = assign(socket, assigns)
    {dir, basename} = split_path(assigns.path)
    dir = Path.expand(dir)

    files =
      if assigns.current_dir != dir do
        list_files(dir, assigns.extnames, assigns.running_paths)
      else
        assigns.files
      end

    {:ok, assign(socket, files: annotate_matching(files, basename), current_dir: dir)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="h-full flex flex-col">
      <div class="flex space-x-5 items-center mb-4">
        <form class="flex-grow"
          phx-change="set_path"
          <%= if @phx_submit do %>
            phx-submit="<%= @phx_submit %>"
          <% else %>
            onsubmit="return false"
          <% end %>
          <%= if @phx_target, do: "phx-target=#{@phx_target}" %>>
          <input class="input"
            id="input-path"
            phx-hook="FocusOnUpdate"
            type="text"
            name="path"
            placeholder="File"
            value="<%= @path %>"
            spellcheck="false"
            autocomplete="off" />
        </form>
        <%= if @inner_block do %>
          <div>
            <%= render_block(@inner_block) %>
          </div>
        <% end %>
      </div>
      <div class="flex-grow -m-1 p-1 overflow-y-auto tiny-scrollbar" tabindex="-1">
        <%= if highlighting?(@files) do %>
          <div class="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-2 border-b border-dashed border-grey-200 mb-2 pb-2">
            <%= for file <- @files, file.highlighted != "" do %>
              <%= render_file(file, @phx_target) %>
            <% end %>
          </div>
        <% end %>

        <div class="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-4 gap-2">
          <%= for file <- @files, file.highlighted == "" do %>
            <%= render_file(file, @phx_target) %>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  defp highlighting?(files) do
    Enum.any?(files, &(&1.highlighted != ""))
  end

  defp render_file(file, phx_target) do
    icon =
      case file do
        %{is_running: true} -> "play-circle-line"
        %{is_dir: true} -> "folder-fill"
        _ -> "file-code-line"
      end

    assigns = %{file: file, icon: icon}

    ~L"""
    <button class="flex space-x-2 items-center p-2 rounded-lg hover:bg-gray-100 focus:ring-1 focus:ring-gray-400"
      phx-click="set_path"
      phx-value-path="<%= @file.path %>"
      <%= if phx_target, do: "phx-target=#{phx_target}" %>>
      <span class="block">
        <%= remix_icon(@icon, class: "text-xl align-middle #{if(@file.is_running, do: "text-green-300", else: "text-gray-400")}") %>
      </span>
      <span class="flex font-medium overflow-hidden overflow-ellipsis whitespace-nowrap <%= if(@file.is_running, do: "text-green-300", else: "text-gray-500") %>">
        <%= if @file.highlighted != "" do %>
          <span class="font-medium <%= if(@file.is_running, do: "text-green-400", else: "text-gray-900") %>">
            <%= @file.highlighted %>
          </span>
        <% end %>
        <span>
          <%= @file.unhighlighted %>
        </span>
      </span>
    </button>
    """
  end

  defp annotate_matching(files, prefix) do
    for %{name: name} = file <- files do
      if String.starts_with?(name, prefix) do
        %{file | highlighted: prefix, unhighlighted: String.replace_prefix(name, prefix, "")}
      else
        %{file | highlighted: "", unhighlighted: name}
      end
    end
  end

  defp list_files(dir, extnames, running_paths) do
    if File.exists?(dir) do
      file_names =
        case File.ls(dir) do
          {:ok, names} -> names
          {:error, _} -> []
        end

      file_infos =
        file_names
        |> Enum.map(fn name ->
          file_info(name, Path.join(dir, name), running_paths)
        end)
        |> Enum.filter(fn file ->
          not hidden?(file.name) and (file.is_dir or valid_extension?(file.name, extnames))
        end)

      parent = Path.dirname(dir)

      file_infos =
        if parent == dir do
          file_infos
        else
          [file_info("..", parent, running_paths) | file_infos]
        end

      Enum.sort_by(file_infos, fn file -> {!file.is_dir, file.name} end)
    else
      []
    end
  end

  defp file_info(name, path, running_paths) do
    is_dir = File.dir?(path)

    %{
      name: name,
      highlighted: "",
      unhighlighted: name,
      path: if(is_dir, do: ensure_trailing_slash(path), else: path),
      is_dir: is_dir,
      is_running: path in running_paths
    }
  end

  defp hidden?(filename) do
    String.starts_with?(filename, ".")
  end

  defp valid_extension?(filename, extnames) do
    Path.extname(filename) in extnames
  end

  # Note: to provide an intuitive behavior when typing the path
  # we enter a new directory when it has a trailing slash,
  # so given "/foo/bar" we list files in "foo" and given "/foo/bar/
  # we list files in "bar".
  #
  # The basename is kinda like search within the current directory,
  # so we highlight files starting with that string.
  defp split_path(path) do
    if String.ends_with?(path, "/") do
      {path, ""}
    else
      {Path.dirname(path), Path.basename(path)}
    end
  end

  defp ensure_trailing_slash(path) do
    if String.ends_with?(path, "/") do
      path
    else
      path <> "/"
    end
  end
end

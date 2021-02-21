defmodule LiveBookWeb.PathSelectComponent do
  use LiveBookWeb, :live_component

  # The component expects:
  #
  # * `path` - the currently entered path
  # * `running_paths` - the list of notebook paths that are already linked to running sessions
  # * `target` - id of the component to send update events to or nil to send to the parent LV
  #
  # The target receives `set_path` events with `%{"path" => path}` payload.

  alias LiveBook.LiveMarkdown

  @impl true
  def render(assigns) do
    ~L"""
    <form phx-change="set_path" phx-submit="set_path" <%= if @target, do: "phx-target=#{@target}" %>>
      <input class="input-base shadow"
        id="input-path"
        phx-hook="FocusOnUpdate"
        type="text"
        name="path"
        placeholder="File"
        value="<%= @path %>"
        spellcheck="false" />
    </form>
    <div class="h-80 -m-1 p-1 overflow-y-auto tiny-scrollbar">
      <div class="grid grid-cols-4 gap-2">
        <%= for file <- list_matching_files(@path, @running_paths) do %>
          <%= render_file(file, @target) %>
        <% end %>
      </div>
    </div>
    """
  end

  defp render_file(file, target) do
    icon =
      case file do
        %{is_running: true} -> :play
        %{is_dir: true} -> :folder
        _ -> :document_text
      end

    assigns = %{file: file, icon: icon}

    ~L"""
    <button class="flex space-x-2 items-center p-2 rounded-md hover:bg-gray-100 focus:ring-1 focus:ring-blue-400 <%= if(@file.is_running, do: "text-green-400 opacity-75", else: "text-gray-700") %>"
      phx-click="set_path"
      phx-value-path="<%= file.path %>"
      <%= if target, do: "phx-target=#{target}" %>>
      <span class="block">
        <%= Icons.svg(@icon, class: "h-5") %>
      </span>
      <span class="block overflow-hidden overflow-ellipsis whitespace-nowrap">
        <%= file.name %>
      </span>
    </button>
    """
  end

  defp list_matching_files(path, running_paths) do
    # Note: to provide an intuitive behavior when typing the path
    # we enter a new directory when it has a trailing slash,
    # so given "/foo/bar" we list files in "foo" and given "/foo/bar/
    # we list files in "bar".
    #
    # The basename is kinda like search within the current directory,
    # so we show only files starting with that string.

    {dir, basename} = split_path(path)
    dir = Path.expand(dir)

    if File.exists?(dir) do
      file_names =
        case File.ls(dir) do
          {:ok, names} -> names
          {:error, _} -> []
        end

      file_infos =
        file_names
        |> Enum.map(fn name ->
          path = Path.join(dir, name)
          is_dir = File.dir?(path)

          %{
            name: name,
            path: if(is_dir, do: ensure_trailing_slash(path), else: path),
            is_dir: is_dir,
            is_running: path in running_paths
          }
        end)
        |> Enum.filter(fn file ->
          not hidden?(file.name) and String.starts_with?(file.name, basename) and
            (file.is_dir or notebook_file?(file.name))
        end)
        |> Enum.sort_by(fn file -> {!file.is_dir, file.name} end)

      if dir == "/" or basename != "" do
        file_infos
      else
        parent_dir = %{
          name: "..",
          path: dir |> Path.join("..") |> Path.expand() |> ensure_trailing_slash(),
          is_dir: true,
          is_running: false
        }

        [parent_dir | file_infos]
      end
    else
      []
    end
  end

  defp hidden?(filename) do
    String.starts_with?(filename, ".")
  end

  defp notebook_file?(filename) do
    String.ends_with?(filename, LiveMarkdown.extension())
  end

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

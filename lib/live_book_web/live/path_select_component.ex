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
    {dir, basename} = split_path(path)

    if File.exists?(dir) do
      file_infos =
        File.ls!(dir)
        |> Enum.map(fn name ->
          path = dir |> Path.join(name) |> Path.expand()
          is_dir = File.dir?(path)

          %{
            name: name,
            path: if(is_dir, do: path <> "/", else: path),
            is_dir: is_dir,
            is_running: path in running_paths
          }
        end)
        |> Enum.reject(&String.starts_with?(&1.name, "."))
        |> Enum.filter(fn file ->
          file.is_dir or String.ends_with?(file.name, LiveMarkdown.extension())
        end)
        |> Enum.filter(&String.starts_with?(&1.name, basename))
        |> Enum.sort_by(fn file -> {!file.is_dir, file.name} end)

      parent_dir = %{
        name: "..",
        path: (dir |> Path.join("..") |> Path.expand()) <> "/",
        is_dir: true,
        is_running: false
      }

      [parent_dir | file_infos]
    else
      []
    end
  end

  defp split_path(path) do
    if File.dir?(path) do
      {path, ""}
    else
      {Path.dirname(path), Path.basename(path)}
    end
  end
end

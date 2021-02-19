defmodule LiveBookWeb.PathSelectComponent do
  use LiveBookWeb, :live_component

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

  defp render_file(%{is_running: true} = file, _target) do
    assigns = %{file: file}

    ~L"""
    <button class="flex space-x-2 items-center p-2 rounded-md text-green-400 opacity-75 cursor-default" tabindex="-1">
      <span class="block">
        <%= Icons.svg(:play, class: "h-5") %>
      </span>
      <span class="block overflow-hidden overflow-ellipsis whitespace-nowrap">
        <%= file.name %>
      </span>
    </button>
    """
  end

  defp render_file(file, target) do
    assigns = %{file: file}

    ~L"""
    <button class="flex space-x-2 items-center p-2 rounded-md text-gray-700 hover:bg-gray-100 hover:text-gray-900 focus:ring-1 focus:ring-blue-400"
      phx-click="set_path"
      phx-value-path="<%= file.path %>"
      <%= if target, do: "phx-target=#{target}" %>>
      <span class="block">
        <%= if(file.is_dir, do: :folder, else: :document_text) |> Icons.svg(class: "h-5") %>
      </span>
      <span class="block overflow-hidden overflow-ellipsis whitespace-nowrap">
        <%= file.name %>
      </span>
    </button>
    """
  end

  defp list_matching_files(path, running_paths) do
    {dir, basename} =
      if File.dir?(path) do
        {path, ""}
      else
        {Path.dirname(path), Path.basename(path)}
      end

    if File.exists?(dir) do
      files =
        File.ls!(dir)
        |> Enum.map(fn name ->
          path = Path.join(dir, name)
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
          file.is_dir or String.ends_with?(file.name, ".livemd")
        end)
        |> Enum.filter(&String.starts_with?(&1.name, basename))
        |> Enum.sort_by(fn file -> {!file.is_dir, file.name} end)

      parent_dir = %{
        name: "..",
        path: (dir |> Path.join("..") |> Path.expand()) <> "/",
        is_dir: true,
        is_running: false
      }

      [parent_dir | files]
    else
      []
    end
  end
end

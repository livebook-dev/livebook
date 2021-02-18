defmodule LiveBookWeb.HomeLive do
  use LiveBookWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    cwd = File.cwd!()
    {:ok, assign(socket, path: cwd)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="container p-4 flex flex-col items-center">
      <h1 class="text-2xl">Welcome to LiveBook</h1>
      <div class="container max-w-4xl mt-8 flex flex-col space-y-4">
        <form phx-change="set_path">
          <input class="input-base shadow"
            type="text"
            name="path"
            placeholder="File"
            value="<%= @path %>"
            spellcheck="false" />
        </form>
        <div class="grid grid-cols-4 gap-2">
          <%= for {filename, filepath} <- list_files(@path) do %>
            <button class="flex space-x-2 items-center p-2 rounded-md cursor-pointer text-gray-700 hover:bg-gray-100 hover:text-gray-900"
              phx-click="set_path"
              phx-value-path="<%= filepath %>">
              <span class="block">
                <%= if File.dir?(filepath) do %>
                  <%= Icons.svg(:folder, class: "h-5") %>
                <% else %>
                  <%= Icons.svg(:document_text, class: "h-5") %>
                <% end %>
              </span>
              <span class="block overflow-hidden overflow-ellipsis whitespace-nowrap">
                <%= filename %>
              </span>
            </button>
          <% end %>
        </div>
        <div class="flex justify-end space-x-2">
          <button class="button-base button-sm">
            Import
          </button>
          <button class="button-base button-primary button-sm">
            Open
          </button>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("set_path", %{"path" => path}, socket) do
    {:noreply, assign(socket, path: path)}
  end

  defp list_files(path) do
    {dir, basename} =
      if File.dir?(path) do
        {path, ""}
      else
        {Path.dirname(path), Path.basename(path)}
      end

    if File.exists?(dir) do
      files =
        File.ls!(dir)
        |> Enum.reject(&String.starts_with?(&1, "."))
        |> Enum.filter(fn name ->
          name |> String.downcase() |> String.starts_with?(String.downcase(basename))
        end)
        |> Enum.sort()
        |> Enum.map(fn name ->
          {name, Path.join(dir, name)}
        end)

      parent = dir |> Path.join("..") |> Path.expand()
      files = [{"..", parent} | files]

      files
    else
      []
    end
  end
end

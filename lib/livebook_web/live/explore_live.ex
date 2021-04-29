defmodule LivebookWeb.ExploreLive do
  use LivebookWeb, :live_view

  alias Livebook.{SessionSupervisor, ContentLoader, LiveMarkdown}

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, notebooks: default_notebooks())}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex flex-grow h-full">
      <div class="flex-grow px-6 py-8 overflow-y-auto">
        <div class="max-w-screen-lg w-full mx-auto p-4 pt-0 pb-8 flex flex-col items-center space-y-4">
          <div class="w-full flex flex-col space-y-2 items-center sm:flex-row sm:space-y-0 sm:justify-between sm:pb-4 pb-8 border-b border-gray-200">
            <div class="text-2xl text-gray-800 font-semibold">
                  <img src="/logo-with-text.png" class="h-[50px]" alt="Livebook" />
            </div>
          </div>
          <div class="w-full py-12">
            <h1 class="text-4xl font-semibold text-gray-800 mb-5">
              Explore
            </h1>
            <%= if @notebooks == [] do %>
              <div class="p-5 flex space-x-4 items-center border border-gray-200 rounded-lg">
                <div>
                  <%= remix_icon("windy-line", class: "text-gray-400 text-xl") %>
                </div>
                <div class="text-gray-600">
                  Whoops! You do not have any notebooks set to explore.
                  <br>
                  Please set one by editing the <span class="font-semibold">“explore.json”</span> file in the project root directory.
                </div>
              </div>
            <% else %>
              <%= for notebook <- @notebooks do %>
                <%= live_component @socket, LivebookWeb.ExploreLive.NotebookComponent, notebook: notebook %>
              <% end %>
            <% end %>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("open", %{"path" => path, "type" => type}, socket) do
    {notebook, messages} = import_notebook({path, type})
    socket = put_import_flash_messages(socket, messages)
    create_session(socket, notebook: notebook)
  end

  defp default_notebooks do
    "explore.json"
    |> rewrite_path
    |> File.read!()
    |> Jason.decode!(keys: :atoms)
  end

  defp create_session(socket, opts \\ []) do
    case SessionSupervisor.create_session(opts) do
      {:ok, id} ->
        {:noreply, push_redirect(socket, to: Routes.session_path(socket, :page, id))}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to create a notebook: #{reason}")}
    end
  end

  defp rewrite_path(path) do
    Path.join(Livebook.Config.root_path(), path)
  end

  defp import_notebook({path, "path"}) do
    path
    |> rewrite_path
    |> File.read!()
    |> LiveMarkdown.Import.notebook_from_markdown()
  end

  defp import_notebook({url, "url"}) do
    {:ok, notebook} =
      url
      |> ContentLoader.rewrite_url()
      |> ContentLoader.fetch_content()

    LiveMarkdown.Import.notebook_from_markdown(notebook)
  end

  defp put_import_flash_messages(socket, []), do: socket

  defp put_import_flash_messages(socket, messages) do
    list =
      messages
      |> Enum.map(fn message -> ["- ", message] end)
      |> Enum.intersperse("\n")

    flash =
      IO.iodata_to_binary([
        "We found problems while importing the file and tried to autofix them:\n" | list
      ])

    put_flash(socket, :info, flash)
  end
end

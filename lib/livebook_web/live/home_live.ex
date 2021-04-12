defmodule LivebookWeb.HomeLive do
  use LivebookWeb, :live_view

  alias Livebook.{SessionSupervisor, Session, LiveMarkdown}

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions")
    end

    session_summaries = sort_session_summaries(SessionSupervisor.get_session_summaries())

    {:ok, assign(socket, path: default_path(), session_summaries: session_summaries)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex flex-grow h-full">
      <div class="flex-grow px-6 py-8 overflow-y-auto">
        <div class="max-w-screen-lg w-full mx-auto p-4 pt-0 pb-8 flex flex-col items-center space-y-4">
          <div class="w-full flex items-center justify-between pb-4 border-b border-gray-200">
            <div class="text-2xl text-gray-800 font-semibold">
              <img src="/logo-with-text.png" class="h-[50px]" alt="Livebook" />
            </div>
            <div class="flex space-x-2 pt-2">
              <span class="tooltip top" aria-label="Introduction">
                <button class="button button-outlined-gray button-square-icon"
                  phx-click="open_welcome">
                  <%= remix_icon("compass-line") %>
                </button>
              </span>
              <button class="button button-blue"
                phx-click="new">
                New notebook
              </button>
            </div>
          </div>
          <div class="w-full h-80">
            <%= live_component @socket, LivebookWeb.PathSelectComponent,
              id: "path_select",
              path: @path,
              extnames: [LiveMarkdown.extension()],
              running_paths: paths(@session_summaries),
              phx_target: nil,
              phx_submit: nil do %>
              <div class="flex justify-end space-x-2">
                <%= content_tag :button,
                  class: "button button-outlined-gray",
                  phx_click: "fork",
                  disabled: not path_forkable?(@path) do %>
                  <%= remix_icon("git-branch-line", class: "align-middle mr-1") %>
                  <span>Fork</span>
                <% end %>
                <%= if path_running?(@path, @session_summaries) do %>
                  <%= live_patch "Join session", to: Routes.session_path(@socket, :page, session_id_by_path(@path, @session_summaries)),
                    class: "button button-blue" %>
                <% else %>
                  <%= content_tag :button, "Open",
                    class: "button button-blue",
                    phx_click: "open",
                    disabled: not path_openable?(@path, @session_summaries) %>
                <% end %>
              </div>
            <% end %>
          </div>
          <div class="w-full py-12">
            <h3 class="text-xl font-semibold text-gray-800 mb-5">
              Running sessions
            </h3>
            <%= if @session_summaries == [] do %>
              <div class="p-5 flex space-x-4 items-center border border-gray-200 rounded-lg">
                <div>
                  <%= remix_icon("windy-line", class: "text-gray-400 text-xl") %>
                </div>
                <div class="text-gray-600">
                  You do not have any running sessions.
                  <br>
                  Please create a new one by clicking <span class="font-semibold">“New notebook”</span>
                </div>
              </div>
            <% else %>
              <%= live_component @socket, LivebookWeb.SessionLive.SessionsComponent,
                id: "sessions_list",
                session_summaries: @session_summaries %>
            <% end %>
          </div>
        </div>
      </div>
    </div>

    <%= if @live_action == :delete_session do %>
      <%= live_modal @socket, LivebookWeb.SessionLive.DeleteSessionComponent,
            id: :delete_session_modal,
            return_to: Routes.home_path(@socket, :page),
            session_summary: @session_summary %>
    <% end %>
    """
  end

  @impl true
  def handle_params(%{"session_id" => session_id}, _url, socket) do
    session_summary = Enum.find(socket.assigns.session_summaries, &(&1.session_id == session_id))
    {:noreply, assign(socket, session_summary: session_summary)}
  end

  def handle_params(_params, _url, socket), do: {:noreply, socket}

  @impl true
  def handle_event("set_path", %{"path" => path}, socket) do
    {:noreply, assign(socket, path: path)}
  end

  def handle_event("open_welcome", %{}, socket) do
    create_session(socket, notebook: Livebook.Notebook.Welcome.new())
  end

  def handle_event("new", %{}, socket) do
    create_session(socket)
  end

  def handle_event("fork", %{}, socket) do
    {notebook, messages} = import_notebook(socket.assigns.path)
    socket = put_import_flash_messages(socket, messages)
    notebook = %{notebook | name: notebook.name <> " - fork"}
    images_dir = Session.images_dir_for_notebook(socket.assigns.path)
    create_session(socket, notebook: notebook, copy_images_from: images_dir)
  end

  def handle_event("open", %{}, socket) do
    {notebook, messages} = import_notebook(socket.assigns.path)
    socket = put_import_flash_messages(socket, messages)
    create_session(socket, notebook: notebook, path: socket.assigns.path)
  end

  def handle_event("fork_session", %{"id" => session_id}, socket) do
    data = Session.get_data(session_id)
    notebook = %{data.notebook | name: data.notebook.name <> " - fork"}
    %{images_dir: images_dir} = Session.get_summary(session_id)
    create_session(socket, notebook: notebook, copy_images_from: images_dir)
  end

  @impl true
  def handle_info({:session_created, id}, socket) do
    summary = Session.get_summary(id)
    session_summaries = sort_session_summaries([summary | socket.assigns.session_summaries])
    {:noreply, assign(socket, session_summaries: session_summaries)}
  end

  def handle_info({:session_deleted, id}, socket) do
    session_summaries = Enum.reject(socket.assigns.session_summaries, &(&1.session_id == id))
    {:noreply, assign(socket, session_summaries: session_summaries)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp default_path(), do: File.cwd!() <> "/"

  defp sort_session_summaries(session_summaries) do
    Enum.sort_by(session_summaries, & &1.notebook_name)
  end

  defp paths(session_summaries) do
    Enum.map(session_summaries, & &1.path)
  end

  defp path_forkable?(path) do
    File.regular?(path)
  end

  defp path_openable?(path, session_summaries) do
    File.regular?(path) and not path_running?(path, session_summaries)
  end

  defp path_running?(path, session_summaries) do
    running_paths = paths(session_summaries)
    path in running_paths
  end

  defp create_session(socket, opts \\ []) do
    case SessionSupervisor.create_session(opts) do
      {:ok, id} ->
        {:noreply, push_redirect(socket, to: Routes.session_path(socket, :page, id))}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to create a notebook: #{reason}")}
    end
  end

  defp import_notebook(path) do
    content = File.read!(path)
    LiveMarkdown.Import.notebook_from_markdown(content)
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

  defp session_id_by_path(path, session_summaries) do
    summary = Enum.find(session_summaries, &(&1.path == path))
    summary.session_id
  end
end

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
    <header class="flex justify-center p-4 border-b">
      <h1 class="text-2xl font-medium">Livebook</h1>
    </header>
    <div class="container max-w-5xl w-full mx-auto p-4 pb-8 flex flex-col items-center space-y-4">
      <div class="w-full flex justify-end">
        <button class="button-base button-sm"
          phx-click="new">
          New notebook
        </button>
      </div>
      <div class="container flex flex-col space-y-4">
        <%= live_component @socket, LivebookWeb.PathSelectComponent,
          id: "path_select",
          path: @path,
          extnames: [LiveMarkdown.extension()],
          running_paths: paths(@session_summaries),
          target: nil %>
        <div class="flex justify-end space-x-2">
          <%= content_tag :button, "Fork",
            class: "button-base button-sm",
            phx_click: "fork",
            disabled: not path_forkable?(@path) %>
          <%= if path_running?(@path, @session_summaries) do %>
            <%= live_patch "Join session", to: Routes.session_path(@socket, :page, session_id_by_path(@path, @session_summaries)),
              class: "button-base button-sm button-primary" %>
          <% else %>
            <%= content_tag :button, "Open",
              class: "button-base button-sm button-primary",
              phx_click: "open",
              disabled: not path_openable?(@path, @session_summaries) %>
          <% end %>
        </div>
      </div>
      <div class="w-full pt-24">
        <h3 class="text-xl font-medium text-gray-900">
          Running sessions
        </h3>
        <%= if @session_summaries == [] do %>
          <div class="mt-3 text-gray-500 text-medium">
            No sessions currently running, you can create one above.
          </div>
        <% else %>
          <%= live_component @socket, LivebookWeb.SessionsComponent,
            id: "sessions_list",
            session_summaries: @session_summaries %>
        <% end %>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("set_path", %{"path" => path}, socket) do
    {:noreply, assign(socket, path: path)}
  end

  def handle_event("new", %{}, socket) do
    create_session(socket)
  end

  def handle_event("fork", %{}, socket) do
    {notebook, messages} = import_notebook(socket.assigns.path)
    socket = put_import_flash_messages(socket, messages)
    notebook = %{notebook | name: notebook.name <> " - fork"}
    create_session(socket, notebook: notebook)
  end

  def handle_event("open", %{}, socket) do
    {notebook, messages} = import_notebook(socket.assigns.path)
    socket = put_import_flash_messages(socket, messages)
    create_session(socket, notebook: notebook, path: socket.assigns.path)
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

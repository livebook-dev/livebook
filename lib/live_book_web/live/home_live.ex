defmodule LiveBookWeb.HomeLive do
  use LiveBookWeb, :live_view

  alias LiveBook.{SessionSupervisor, Session}

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions")
    end

    session_summaries = sort_session_summaries(SessionSupervisor.get_session_summaries())

    {:ok,
     assign(socket, path: default_path(), path_valid: false, session_summaries: session_summaries)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <header class="flex justify-center p-4 border-b">
      <h1 class="text-2xl font-medium">LiveBook</h1>
    </header>
    <div class="mt-4 container max-w-4xl w-full mx-auto flex flex-col items-center space-y-4 pb-8">
      <div class="w-full flex justify-end">
        <button class="button-base button-sm"
          phx-click="new">
          New notebook
        </button>
      </div>
      <div class="container flex flex-col space-y-4">
        <%= live_component @socket, LiveBookWeb.PathSelectComponent,
          id: "path_select",
          path: @path,
          running_paths: paths(@session_summaries),
          target: nil %>
        <div class="flex justify-end space-x-2">
          <%= content_tag :button, "Import",
            class: "button-base button-sm",
            phx_click: "import",
            disabled: !@path_valid %>
          <%= content_tag :button, "Open",
            class: "button-base button-sm button-primary",
            phx_click: "open",
            disabled: !@path_valid %>
        </div>
      </div>
      <div class="w-full pt-24">
        <%= live_component @socket, LiveBookWeb.SessionsComponent,
          id: "sessions_list",
          session_summaries: @session_summaries %>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("set_path", %{"path" => path}, socket) do
    path_valid = path_valid?(path, paths(socket.assigns.session_summaries))
    {:noreply, assign(socket, path: path, path_valid: path_valid)}
  end

  def handle_event("new", %{}, socket) do
    create_session(socket)
  end

  def handle_event("import", %{}, socket) do
    create_session(socket, import_path: socket.assigns.path, keep_path: false)
  end

  def handle_event("open", %{}, socket) do
    create_session(socket, import_path: socket.assigns.path, keep_path: true)
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

  defp path_valid?(path, running_paths) do
    File.regular?(path) and path not in running_paths
  end

  defp create_session(socket, opts \\ []) do
    case SessionSupervisor.create_session(opts) do
      {:ok, id} ->
        {:noreply, push_redirect(socket, to: Routes.session_path(socket, :page, id))}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to create a notebook: #{reason}")}
    end
  end
end

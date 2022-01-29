defmodule LivebookWeb.HomeLive do
  use LivebookWeb, :live_view

  import LivebookWeb.SessionHelpers
  import LivebookWeb.UserHelpers

  alias LivebookWeb.{SidebarHelpers, ExploreHelpers}
  alias Livebook.{Sessions, Session, LiveMarkdown, Notebook, FileSystem}

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "tracker_sessions")
    end

    sessions = Sessions.list_sessions()
    notebook_infos = Notebook.Explore.visible_notebook_infos() |> Enum.take(3)

    {:ok,
     socket
     |> SidebarHelpers.shared_home_handlers()
     |> assign(
       file: Livebook.Config.default_dir(),
       file_info: %{exists: true, access: :read_write},
       sessions: sessions,
       notebook_infos: notebook_infos,
       page_title: "Livebook"
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex grow h-full">
      <SidebarHelpers.sidebar>
        <SidebarHelpers.shared_home_footer
          socket={@socket}
          current_user={@current_user}
          user_path={Routes.home_path(@socket, :user)} />
      </SidebarHelpers.sidebar>
      <div class="grow px-6 py-8 overflow-y-auto">
        <div class="max-w-screen-lg w-full mx-auto px-4 pb-8 space-y-4">
          <div class="flex flex-col space-y-2 items-center pb-4 border-b border-gray-200
                      sm:flex-row sm:space-y-0 sm:justify-between">
            <div class="text-2xl text-gray-800 font-semibold">
              <img src="/images/logo-with-text.png" class="h-[50px]" alt="Livebook" />
            </div>
            <div class="flex space-x-2 pt-2">
              <%= live_patch "Import",
                    to: Routes.home_path(@socket, :import, "url"),
                    class: "button-base button-outlined-gray whitespace-nowrap" %>
              <button class="button-base button-blue" phx-click="new">
                New notebook
              </button>
            </div>
          </div>

          <div class="h-80">
            <.live_component module={LivebookWeb.FileSelectComponent}
                id="home-file-select"
                file={@file}
                extnames={[LiveMarkdown.extension()]}
                running_files={files(@sessions)}>
              <div class="flex justify-end space-x-2">
                <button class="button-base button-outlined-gray whitespace-nowrap"
                  phx-click="fork"
                  disabled={not path_forkable?(@file, @file_info)}>
                  <.remix_icon icon="git-branch-line" class="align-middle mr-1" />
                  <span>Fork</span>
                </button>
                <%= if file_running?(@file, @sessions) do %>
                  <%= live_redirect "Join session",
                        to: Routes.session_path(@socket, :page, session_id_by_file(@file, @sessions)),
                        class: "button-base button-blue" %>
                <% else %>
                  <span {open_button_tooltip_attrs(@file, @file_info)}>
                    <button class="button-base button-blue"
                      phx-click="open"
                      disabled={not path_openable?(@file, @file_info, @sessions)}>
                      Open
                    </button>
                  </span>
                <% end %>
              </div>
            </.live_component>
          </div>

          <div class="py-12" data-element="explore-section">
            <div class="mb-4 flex justify-between items-center">
              <h2 class="uppercase font-semibold text-gray-500">
                Explore
              </h2>
              <%= live_redirect to: Routes.explore_path(@socket, :page),
                    class: "flex items-center text-blue-600" do %>
                <span class="font-semibold">See all</span>
                <.remix_icon icon="arrow-right-line" class="align-middle ml-1" />
              <% end %>
            </div>
            <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
              <%# Note: it's fine to use stateless components in this comprehension,
                  because @notebook_infos never change %>
              <%= for info <- @notebook_infos do %>
                <ExploreHelpers.notebook_card notebook_info={info} socket={@socket} />
              <% end %>
            </div>
          </div>
          <div class="py-12">
            <.live_component module={LivebookWeb.HomeLive.SessionListComponent}
              id="session-list"
              sessions={@sessions}/>
          </div>
        </div>
      </div>
    </div>

    <%= if @live_action == :user do %>
      <.current_user_modal
        return_to={Routes.home_path(@socket, :page)}
        current_user={@current_user} />
    <% end %>

    <%= if @live_action == :close_session do %>
      <.modal class="w-full max-w-xl" return_to={Routes.home_path(@socket, :page)}>
        <.live_component module={LivebookWeb.HomeLive.CloseSessionComponent}
          id="close-session"
          return_to={Routes.home_path(@socket, :page)}
          session={@session} />
      </.modal>
    <% end %>

    <%= if @live_action == :import do %>
      <.modal class="w-full max-w-xl" return_to={Routes.home_path(@socket, :page)}>
        <.live_component module={LivebookWeb.HomeLive.ImportComponent}
          id="import"
          tab={@tab}
          import_opts={@import_opts} />
      </.modal>
    <% end %>

    <%= if @live_action == :edit_sessions do %>
      <.modal class="w-full max-w-xl" return_to={Routes.home_path(@socket, :page)}>
        <.live_component module={LivebookWeb.HomeLive.EditSessionsComponent}
          id="edit-sessions"
          action={@bulk_action}
          return_to={Routes.home_path(@socket, :page)}
          sessions={@sessions}
          selected_sessions={selected_sessions(@sessions, @selected_session_ids)} />
      </.modal>
    <% end %>
    """
  end

  defp open_button_tooltip_attrs(file, file_info) do
    if regular?(file, file_info) and not writable?(file_info) do
      [class: "tooltip top", data_tooltip: "This file is write-protected, please fork instead"]
    else
      []
    end
  end

  @impl true
  def handle_params(%{"session_id" => session_id}, _url, socket) do
    session = Enum.find(socket.assigns.sessions, &(&1.id == session_id))
    {:noreply, assign(socket, session: session)}
  end

  def handle_params(
        %{"action" => action},
        _url,
        %{assigns: %{live_action: :edit_sessions}} = socket
      ) do
    {:noreply, assign(socket, bulk_action: action)}
  end

  def handle_params(%{"tab" => tab} = params, _url, %{assigns: %{live_action: :import}} = socket) do
    import_opts = [url: params["url"]]
    {:noreply, assign(socket, tab: tab, import_opts: import_opts)}
  end

  def handle_params(%{"url" => url}, _url, %{assigns: %{live_action: :public_import}} = socket) do
    origin = Livebook.ContentLoader.url_to_location(url)

    origin
    |> Livebook.ContentLoader.fetch_content_from_location()
    |> case do
      {:ok, content} ->
        socket = import_content(socket, content, origin: origin)
        {:noreply, socket}

      {:error, _message} ->
        {:noreply, push_patch(socket, to: Routes.home_path(socket, :import, "url", url: url))}
    end
  end

  def handle_params(_params, _url, socket), do: {:noreply, socket}

  @impl true
  def handle_event("new", %{}, socket) do
    {:noreply, create_session(socket)}
  end

  def handle_event("fork", %{}, socket) do
    file = socket.assigns.file

    socket =
      case import_notebook(file) do
        {:ok, {notebook, messages}} ->
          notebook = Notebook.forked(notebook)
          images_dir = Session.images_dir_for_notebook(file)

          socket
          |> put_import_warnings(messages)
          |> create_session(
            notebook: notebook,
            copy_images_from: images_dir,
            origin: {:file, file}
          )

        {:error, error} ->
          put_flash(socket, :error, Livebook.Utils.upcase_first(error))
      end

    {:noreply, socket}
  end

  def handle_event("open", %{}, socket) do
    file = socket.assigns.file

    socket =
      case import_notebook(file) do
        {:ok, {notebook, messages}} ->
          socket
          |> put_import_warnings(messages)
          |> create_session(notebook: notebook, file: file, origin: {:file, file})

        {:error, error} ->
          put_flash(socket, :error, Livebook.Utils.upcase_first(error))
      end

    {:noreply, socket}
  end

  def handle_event("bulk_action", %{"action" => "disconnect"} = params, socket) do
    socket = assign(socket, selected_session_ids: params["session_ids"])
    {:noreply, push_patch(socket, to: Routes.home_path(socket, :edit_sessions, "disconnect"))}
  end

  def handle_event("bulk_action", %{"action" => "close_all"} = params, socket) do
    socket = assign(socket, selected_session_ids: params["session_ids"])
    {:noreply, push_patch(socket, to: Routes.home_path(socket, :edit_sessions, "close_all"))}
  end

  def handle_event("disconnect_runtime", %{"id" => session_id}, socket) do
    session = Enum.find(socket.assigns.sessions, &(&1.id == session_id))
    Session.disconnect_runtime(session.pid)
    {:noreply, socket}
  end

  def handle_event("fork_session", %{"id" => session_id}, socket) do
    session = Enum.find(socket.assigns.sessions, &(&1.id == session_id))
    %{images_dir: images_dir} = session
    data = Session.get_data(session.pid)
    notebook = Notebook.forked(data.notebook)

    origin =
      if data.file do
        {:file, data.file}
      else
        data.origin
      end

    {:noreply,
     create_session(socket,
       notebook: notebook,
       copy_images_from: images_dir,
       origin: origin
     )}
  end

  def handle_event("open_autosave_directory", %{}, socket) do
    file =
      Livebook.Config.autosave_path()
      |> FileSystem.Utils.ensure_dir_path()
      |> FileSystem.File.local()

    file_info = %{exists: true, access: file_access(file)}
    {:noreply, assign(socket, file: file, file_info: file_info)}
  end

  @impl true
  def handle_info({:set_file, file, info}, socket) do
    file_info = %{exists: info.exists, access: file_access(file)}
    {:noreply, assign(socket, file: file, file_info: file_info)}
  end

  def handle_info({:session_created, session}, socket) do
    if session in socket.assigns.sessions do
      {:noreply, socket}
    else
      {:noreply, assign(socket, sessions: [session | socket.assigns.sessions])}
    end
  end

  def handle_info({:session_updated, session}, socket) do
    sessions =
      Enum.map(socket.assigns.sessions, fn other ->
        if other.id == session.id, do: session, else: other
      end)

    {:noreply, assign(socket, sessions: sessions)}
  end

  def handle_info({:session_closed, session}, socket) do
    sessions = Enum.reject(socket.assigns.sessions, &(&1.id == session.id))
    {:noreply, assign(socket, sessions: sessions)}
  end

  def handle_info({:import_content, content, session_opts}, socket) do
    socket = import_content(socket, content, session_opts)
    {:noreply, socket}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp files(sessions) do
    Enum.map(sessions, & &1.file)
  end

  defp path_forkable?(file, file_info) do
    regular?(file, file_info)
  end

  defp path_openable?(file, file_info, sessions) do
    regular?(file, file_info) and not file_running?(file, sessions) and
      writable?(file_info)
  end

  defp regular?(file, file_info) do
    file_info.exists and not FileSystem.File.dir?(file)
  end

  defp writable?(file_info) do
    file_info.access in [:read_write, :write]
  end

  defp file_running?(file, sessions) do
    running_files = files(sessions)
    file in running_files
  end

  defp import_notebook(file) do
    with {:ok, content} <- FileSystem.File.read(file) do
      {:ok, LiveMarkdown.Import.notebook_from_markdown(content)}
    end
  end

  defp session_id_by_file(file, sessions) do
    session = Enum.find(sessions, &(&1.file == file))
    session.id
  end

  defp import_content(socket, content, session_opts) do
    {notebook, messages} = Livebook.LiveMarkdown.Import.notebook_from_markdown(content)

    socket =
      socket
      |> put_import_warnings(messages)
      |> put_flash(
        :info,
        "You have imported a notebook, no code has been executed so far. You should read and evaluate code as needed."
      )

    session_opts = Keyword.merge(session_opts, notebook: notebook)
    create_session(socket, session_opts)
  end

  defp file_access(file) do
    case FileSystem.File.access(file) do
      {:ok, access} -> access
      {:error, _} -> :none
    end
  end

  defp selected_sessions(sessions, selected_session_ids) do
    Enum.filter(sessions, &(&1.id in selected_session_ids))
  end
end

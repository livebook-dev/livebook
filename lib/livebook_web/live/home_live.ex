defmodule LivebookWeb.HomeLive do
  use LivebookWeb, :live_view

  import LivebookWeb.UserHelpers
  import LivebookWeb.SessionHelpers

  alias LivebookWeb.{SidebarHelpers, ExploreHelpers}
  alias Livebook.{Sessions, Session, LiveMarkdown, Notebook, FileSystem}

  @impl true
  def mount(_params, %{"current_user_id" => current_user_id} = session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "tracker_sessions")
      Phoenix.PubSub.subscribe(Livebook.PubSub, "users:#{current_user_id}")
    end

    current_user = build_current_user(session, socket)
    sessions = sort_sessions(Sessions.list_sessions())
    notebook_infos = Notebook.Explore.notebook_infos() |> Enum.take(3)

    {:ok,
     assign(socket,
       current_user: current_user,
       file: Livebook.Config.default_dir(),
       file_info: %{exists: true, access: :read_write},
       sessions: sessions,
       notebook_infos: notebook_infos
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-grow h-full">
      <SidebarHelpers.sidebar>
        <SidebarHelpers.break_item />
        <SidebarHelpers.link_item
          icon="settings-3-fill"
          label="Settings"
          path={Routes.settings_path(@socket, :page)}
          active={false} />
        <SidebarHelpers.user_item current_user={@current_user} path={Routes.home_path(@socket, :user)} />
      </SidebarHelpers.sidebar>
      <div class="flex-grow px-6 py-8 overflow-y-auto">
        <div class="max-w-screen-lg w-full mx-auto px-4 pb-8 space-y-4">
          <div class="flex flex-col space-y-2 items-center pb-4 border-b border-gray-200
                      sm:flex-row sm:space-y-0 sm:justify-between">
            <div class="text-2xl text-gray-800 font-semibold">
              <img src="/images/logo-with-text.png" class="h-[50px]" alt="Livebook" />
            </div>
            <div class="flex space-x-2 pt-2">
              <%= live_patch "Import",
                    to: Routes.home_path(@socket, :import, "url"),
                    class: "button button-outlined-gray whitespace-nowrap" %>
              <button class="button button-blue" phx-click="new">
                New notebook
              </button>
            </div>
          </div>

          <div class="h-80">
            <%= live_component LivebookWeb.FileSelectComponent,
                  id: "home-file-select",
                  file: @file,
                  extnames: [LiveMarkdown.extension()],
                  running_files: files(@sessions) do %>
              <div class="flex justify-end space-x-2">
                <button class="button button-outlined-gray whitespace-nowrap"
                  phx-click="fork"
                  disabled={not path_forkable?(@file, @file_info)}>
                  <.remix_icon icon="git-branch-line" class="align-middle mr-1" />
                  <span>Fork</span>
                </button>
                <%= if file_running?(@file, @sessions) do %>
                  <%= live_redirect "Join session",
                        to: Routes.session_path(@socket, :page, session_id_by_file(@file, @sessions)),
                        class: "button button-blue" %>
                <% else %>
                  <span {open_button_tooltip_attrs(@file, @file_info)}>
                    <button class="button button-blue"
                      phx-click="open"
                      disabled={not path_openable?(@file, @file_info, @sessions)}>
                      Open
                    </button>
                  </span>
                <% end %>
              </div>
            <% end %>
          </div>

          <div class="py-12">
            <div class="mb-4 flex justify-between items-center">
              <h2 class="text-xl font-semibold text-gray-800">
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
            <h2 class="mb-4 text-xl font-semibold text-gray-800">
              Running sessions
            </h2>
            <.sessions_list sessions={@sessions} socket={@socket} />
          </div>
        </div>
      </div>
    </div>

    <%= if @live_action == :user do %>
      <%= live_modal LivebookWeb.UserComponent,
            id: "user",
            modal_class: "w-full max-w-sm",
            user: @current_user,
            return_to: Routes.home_path(@socket, :page) %>
    <% end %>

    <%= if @live_action == :close_session do %>
      <%= live_modal LivebookWeb.HomeLive.CloseSessionComponent,
            id: "close-session",
            modal_class: "w-full max-w-xl",
            return_to: Routes.home_path(@socket, :page),
            session: @session %>
    <% end %>

    <%= if @live_action == :import do %>
      <%= live_modal LivebookWeb.HomeLive.ImportComponent,
            id: "import",
            modal_class: "w-full max-w-xl",
            return_to: Routes.home_path(@socket, :page),
            tab: @tab,
            import_opts: @import_opts %>
    <% end %>
    """
  end

  defp open_button_tooltip_attrs(file, file_info) do
    if regular?(file, file_info) and not writable?(file_info) do
      [class: "tooltip top", aria_label: "This file is write-protected, please fork instead"]
    else
      []
    end
  end

  defp sessions_list(%{sessions: []} = assigns) do
    ~H"""
    <div class="p-5 flex space-x-4 items-center border border-gray-200 rounded-lg">
      <div>
        <.remix_icon icon="windy-line" class="text-gray-400 text-xl" />
      </div>
      <div class="text-gray-600">
        You do not have any running sessions.
        <br>
        Please create a new one by clicking <span class="font-semibold">“New notebook”</span>
      </div>
    </div>
    """
  end

  defp sessions_list(assigns) do
    ~H"""
    <div class="flex flex-col space-y-4">
      <%= for session <- @sessions do %>
        <div class="p-5 flex items-center border border-gray-200 rounded-lg"
          data-test-session-id={session.id}>
          <div class="flex-grow flex flex-col space-y-1">
            <%= live_redirect session.notebook_name,
                  to: Routes.session_path(@socket, :page, session.id),
                  class: "font-semibold text-gray-800 hover:text-gray-900" %>
            <div class="text-gray-600 text-sm">
              <%= if session.file, do: session.file.path, else: "No file" %>
            </div>
            <div class="text-gray-600 text-sm">
              Created <%= format_creation_date(session.created_at) %>
            </div>
          </div>
          <div class="relative" id={"session-#{session.id}-menu"} phx-hook="Menu" data-element="menu">
            <button class="icon-button" data-toggle>
              <.remix_icon icon="more-2-fill" class="text-xl" />
            </button>
            <div class="menu" data-content>
              <button class="menu__item text-gray-500"
                phx-click="fork_session"
                phx-value-id={session.id}>
                <.remix_icon icon="git-branch-line" />
                <span class="font-medium">Fork</span>
              </button>
              <a class="menu__item text-gray-500"
                href={live_dashboard_process_path(@socket, session.pid)}
                target="_blank">
                <.remix_icon icon="dashboard-2-line" />
                <span class="font-medium">See on Dashboard</span>
              </a>
              <%= live_patch to: Routes.home_path(@socket, :close_session, session.id),
                    class: "menu__item text-red-600" do %>
                <.remix_icon icon="close-circle-line" />
                <span class="font-medium">Close</span>
              <% end %>
            </div>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_params(%{"session_id" => session_id}, _url, socket) do
    session = Enum.find(socket.assigns.sessions, &(&1.id == session_id))
    {:noreply, assign(socket, session: session)}
  end

  def handle_params(%{"tab" => tab} = params, _url, %{assigns: %{live_action: :import}} = socket) do
    import_opts = [url: params["url"]]
    {:noreply, assign(socket, tab: tab, import_opts: import_opts)}
  end

  def handle_params(%{"url" => url}, _url, %{assigns: %{live_action: :public_import}} = socket) do
    url
    |> Livebook.ContentLoader.rewrite_url()
    |> Livebook.ContentLoader.fetch_content()
    |> case do
      {:ok, content} ->
        socket = import_content(socket, content, origin: {:url, url})
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
          |> put_import_flash_messages(messages)
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
          |> put_import_flash_messages(messages)
          |> create_session(notebook: notebook, file: file, origin: {:file, file})

        {:error, error} ->
          put_flash(socket, :error, Livebook.Utils.upcase_first(error))
      end

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

  @impl true
  def handle_info({:set_file, file, info}, socket) do
    file_info = %{
      exists: info.exists,
      access:
        case FileSystem.File.access(file) do
          {:ok, access} -> access
          {:error, _} -> :none
        end
    }

    {:noreply, assign(socket, file: file, file_info: file_info)}
  end

  def handle_info({:session_created, session}, socket) do
    if session in socket.assigns.sessions do
      {:noreply, socket}
    else
      sessions = sort_sessions([session | socket.assigns.sessions])
      {:noreply, assign(socket, sessions: sessions)}
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

  def handle_info(
        {:user_change, %{id: id} = user},
        %{assigns: %{current_user: %{id: id}}} = socket
      ) do
    {:noreply, assign(socket, :current_user, user)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp sort_sessions(sessions) do
    Enum.sort_by(sessions, & &1.created_at, {:desc, DateTime})
  end

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

  def format_creation_date(created_at) do
    time_words = created_at |> DateTime.to_naive() |> Livebook.Utils.Time.time_ago_in_words()
    time_words <> " ago"
  end

  defp import_content(socket, content, session_opts) do
    {notebook, messages} = Livebook.LiveMarkdown.Import.notebook_from_markdown(content)
    socket = put_import_flash_messages(socket, messages)
    session_opts = Keyword.merge(session_opts, notebook: notebook)
    create_session(socket, session_opts)
  end
end

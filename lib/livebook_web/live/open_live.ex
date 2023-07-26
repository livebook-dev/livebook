defmodule LivebookWeb.OpenLive do
  use LivebookWeb, :live_view

  import LivebookWeb.SessionHelpers

  alias LivebookWeb.LayoutHelpers
  alias Livebook.{Sessions, Notebook, FileSystem}

  on_mount LivebookWeb.SidebarHook

  @impl true
  def mount(params, _session, socket) do
    if connected?(socket) do
      Livebook.Sessions.subscribe()
      Livebook.NotebookManager.subscribe_recent_notebooks()
    end

    sessions = Sessions.list_sessions() |> Enum.filter(&(&1.mode == :default))
    recent_notebooks = Livebook.NotebookManager.recent_notebooks()

    {:ok,
     assign(socket,
       tab: "file",
       initial_file: file_from_params(params),
       url: params["url"],
       sessions: sessions,
       recent_notebooks: recent_notebooks,
       page_title: "Open - Livebook"
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <LayoutHelpers.layout current_page={~p"/"} current_user={@current_user} saved_hubs={@saved_hubs}>
      <:topbar_action>
        <.link class="button-base button-blue" navigate={~p"/new"}>
          <.remix_icon icon="add-line" class="align-middle mr-1" />
          <span>New notebook</span>
        </.link>
      </:topbar_action>
      <div class="p-4 md:px-12 md:py-6 max-w-screen-lg mx-auto space-y-4">
        <div class="flex flex-row space-y-0 items-center pb-4 justify-between">
          <LayoutHelpers.title text="Open notebook" back_navigate={~p"/"} />
          <div class="hidden md:flex" role="navigation" aria-label="new notebook">
            <.link class="button-base button-blue" navigate={~p"/new"}>
              <.remix_icon icon="add-line" class="align-middle mr-1" />
              <span>New notebook</span>
            </.link>
          </div>
        </div>

        <div class="tabs">
          <.link patch={~p"/open/file"} class={["tab", @tab == "file" && "active"]}>
            <.remix_icon icon="file-3-line" class="align-middle" />
            <span class="font-medium">From file</span>
          </.link>
          <.link patch={~p"/open/url"} class={["tab", @tab == "url" && "active"]}>
            <.remix_icon icon="download-cloud-2-line" class="align-middle" />
            <span class="font-medium">From URL</span>
          </.link>
          <.link patch={~p"/open/source"} class={["tab", @tab == "source" && "active"]}>
            <.remix_icon icon="clipboard-line" class="align-middle" />
            <span class="font-medium">From source</span>
          </.link>
          <.link patch={~p"/open/upload"} class={["tab", @tab == "upload" && "active"]}>
            <.remix_icon icon="file-upload-line" class="align-middle" />
            <span class="font-medium">File upload</span>
          </.link>
          <div class="grow tab"></div>
        </div>

        <div class="h-96">
          <.live_component
            :if={@tab == "file"}
            module={LivebookWeb.OpenLive.FileComponent}
            id="import-file"
            sessions={@sessions}
            initial_file={@initial_file}
          />
          <.live_component
            :if={@tab == "url"}
            module={LivebookWeb.OpenLive.UrlComponent}
            id="import-url"
            url={@url}
          />
          <.live_component
            :if={@tab == "source"}
            module={LivebookWeb.OpenLive.SourceComponent}
            id="import-source"
          />
          <.live_component
            :if={@tab == "upload"}
            module={LivebookWeb.OpenLive.UploadComponent}
            id="import-upload"
          />
        </div>

        <div id="recent-notebooks" class="pb-10" role="region" aria-label="recent notebooks">
          <div class="mb-4 flex items-center md:items-end justify-between">
            <h2 class="uppercase font-semibold text-gray-500 text-sm md:text-base">
              Recent notebooks
            </h2>
          </div>
          <%= if @recent_notebooks == [] do %>
            <.no_entries>
              Your most recently opened notebooks will appear here.
            </.no_entries>
          <% else %>
            <.live_component
              module={LivebookWeb.NotebookCardsComponent}
              id="recent-notebook-list"
              notebook_infos={@recent_notebooks}
              sessions={@sessions}
              added_at_label="Opened"
            >
              <:card_icon :let={{_info, idx}}>
                <span class="tooltip top" data-tooltip="Hide notebook">
                  <button
                    aria-label="hide notebook"
                    phx-click={JS.push("hide_recent_notebook", value: %{idx: idx})}
                  >
                    <.remix_icon icon="close-fill" class="text-gray-600 text-lg" />
                  </button>
                </span>
              </:card_icon>
            </.live_component>
          <% end %>
          <div class="mt-3 text-gray-600 text-sm">
            Looking for unsaved notebooks? <.link
              class="font-semibold"
              navigate={~p"/open/file?autosave=true"}
              phx-no-format
            >Browse them here</.link>.
          </div>
        </div>
      </div>
    </LayoutHelpers.layout>
    """
  end

  @impl true
  def handle_params(%{"tab" => tab}, _url, socket) when socket.assigns.live_action == :page do
    {:noreply, assign(socket, tab: tab)}
  end

  def handle_params(%{"url" => url}, _url, socket)
      when socket.assigns.live_action == :public_import do
    origin = Notebook.ContentLoader.url_to_location(url)

    origin
    |> Notebook.ContentLoader.fetch_content_from_location()
    |> case do
      {:ok, content} ->
        socket = import_source(socket, content, origin: origin)
        {:noreply, socket}

      {:error, _message} ->
        {:noreply, push_patch(socket, to: ~p"/open/url?url=#{url}")}
    end
  end

  def handle_params(%{"path" => path} = _params, _uri, socket)
      when socket.assigns.live_action == :public_open do
    expanded_path = Path.expand(path)

    if File.dir?(expanded_path) do
      {:noreply, push_patch(socket, to: ~p"/open/file?path=#{path}")}
    else
      file = FileSystem.File.local(expanded_path)

      if file_running?(file, socket.assigns.sessions) do
        session_id = session_id_by_file(file, socket.assigns.sessions)
        {:noreply, push_navigate(socket, to: ~p"/sessions/#{session_id}")}
      else
        {:noreply, open_notebook(socket, file)}
      end
    end
  end

  def handle_params(_params, _url, socket), do: {:noreply, socket}

  @impl true
  def handle_event("hide_recent_notebook", %{"idx" => idx}, socket) do
    on_confirm = fn socket ->
      %{file: file} = Enum.fetch!(socket.assigns.recent_notebooks, idx)
      Livebook.NotebookManager.remove_recent_notebook(file)
      socket
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Hide notebook",
       description: "The notebook will reappear here when you open it again.",
       confirm_text: "Hide",
       opt_out_id: "hide-notebook"
     )}
  end

  @impl true
  def handle_info({type, session} = event, socket)
      when type in [:session_created, :session_updated, :session_closed] and
             session.mode == :default do
    {:noreply, update(socket, :sessions, &update_session_list(&1, event))}
  end

  def handle_info({:fork, file}, socket) do
    {:noreply, fork_notebook(socket, file)}
  end

  def handle_info({:open, file}, socket) do
    {:noreply, open_notebook(socket, file)}
  end

  def handle_info({:import_source, source, session_opts}, socket) do
    socket = import_source(socket, source, session_opts)
    {:noreply, socket}
  end

  def handle_info({:recent_notebooks_updated, recent_notebooks}, socket) do
    {:noreply, assign(socket, recent_notebooks: recent_notebooks)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp file_from_params(%{"autosave" => _} = _params) do
    Livebook.Settings.autosave_path()
    |> FileSystem.Utils.ensure_dir_path()
    |> FileSystem.File.local()
  end

  defp file_from_params(%{"path" => path} = _params) do
    path = Path.expand(path)

    cond do
      File.dir?(path) ->
        path
        |> FileSystem.Utils.ensure_dir_path()
        |> FileSystem.File.local()

      File.regular?(path) ->
        FileSystem.File.local(path)

      true ->
        Livebook.Config.local_file_system_home()
    end
  end

  defp file_from_params(_params), do: Livebook.Settings.default_dir()

  defp import_source(socket, source, session_opts) do
    {notebook, messages} = Livebook.LiveMarkdown.notebook_from_livemd(source)

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

  defp file_running?(file, sessions) do
    Enum.any?(sessions, &(&1.file == file))
  end

  defp session_id_by_file(file, sessions) do
    session = Enum.find(sessions, &(&1.file == file))
    session.id
  end
end

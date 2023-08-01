defmodule LivebookWeb.HomeLive do
  use LivebookWeb, :live_view

  import LivebookWeb.SessionHelpers

  alias LivebookWeb.{LearnHelpers, LayoutHelpers}
  alias Livebook.{Sessions, Notebook}

  on_mount LivebookWeb.SidebarHook

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Livebook.Sessions.subscribe()
      Livebook.SystemResources.subscribe()
      Livebook.NotebookManager.subscribe_starred_notebooks()
    end

    sessions = Sessions.list_sessions() |> Enum.filter(&(&1.mode == :default))
    notebook_infos = Notebook.Learn.visible_notebook_infos() |> Enum.take(3)
    starred_notebooks = Livebook.NotebookManager.starred_notebooks()

    {:ok,
     assign(socket,
       self_path: ~p"/",
       sessions: sessions,
       starred_notebooks: starred_notebooks,
       starred_expanded?: false,
       notebook_infos: notebook_infos,
       page_title: "Livebook",
       new_version: Livebook.UpdateCheck.new_version(),
       update_instructions_url: Livebook.Config.update_instructions_url(),
       app_service_url: Livebook.Config.app_service_url(),
       memory: Livebook.SystemResources.memory()
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <LayoutHelpers.layout
      current_page={@self_path}
      current_user={@current_user}
      saved_hubs={@saved_hubs}
    >
      <:topbar_action>
        <div class="flex space-x-2">
          <.link navigate={~p"/open/file"} class="button-base button-outlined-gray whitespace-nowrap">
            Open
          </.link>
          <.link class="button-base button-blue" patch={~p"/new"}>
            <.remix_icon icon="add-line" class="align-middle mr-1" />
            <span>New notebook</span>
          </.link>
        </div>
      </:topbar_action>

      <.update_notification version={@new_version} instructions_url={@update_instructions_url} />
      <.memory_notification memory={@memory} app_service_url={@app_service_url} />

      <div class="p-4 md:px-12 md:py-6 max-w-screen-lg mx-auto">
        <div class="flex flex-row space-y-0 items-center pb-4 justify-between">
          <LayoutHelpers.title text="Home" />
          <div class="hidden md:flex space-x-2" role="navigation" aria-label="new notebook">
            <.link
              navigate={~p"/open/file"}
              class="button-base button-outlined-gray whitespace-nowrap"
            >
              Open
            </.link>
            <.link class="button-base button-blue" patch={~p"/new"}>
              <.remix_icon icon="add-line" class="align-middle mr-1" />
              <span>New notebook</span>
            </.link>
          </div>
        </div>

        <div id="starred-notebooks" role="region" aria-label="starred notebooks">
          <div class="my-4 flex items-center md:items-end justify-between">
            <h2 class="uppercase font-semibold text-gray-500 text-sm md:text-base">
              Starred notebooks
            </h2>
            <button
              :if={length(@starred_notebooks) > 6}
              class="flex items-center text-blue-600"
              phx-click="toggle_starred_expanded"
            >
              <%= if @starred_expanded? do %>
                <span class="font-semibold">Show less</span>
              <% else %>
                <span class="font-semibold">Show more</span>
              <% end %>
            </button>
          </div>
          <%= if @starred_notebooks == [] do %>
            <.no_entries>
              Your starred notebooks will appear here. <br />
              First time around? Check out the notebooks below to get started.
              <:actions>
                <.link navigate={~p"/learn"} class="flex items-center text-blue-600 pl-5">
                  <span class="font-semibold">Learn more</span>
                  <.remix_icon icon="arrow-right-line" class="align-middle ml-1" />
                </.link>
              </:actions>
            </.no_entries>
            <div class="mt-4 grid grid-cols-1 md:grid-cols-3 gap-4">
              <% # Note: it's fine to use stateless components in this comprehension,
              # because @notebook_infos never change %>
              <LearnHelpers.notebook_card :for={info <- @notebook_infos} notebook_info={info} />
            </div>
          <% else %>
            <.live_component
              module={LivebookWeb.NotebookCardsComponent}
              id="starred-notebook-list"
              notebook_infos={visible_starred_notebooks(@starred_notebooks, @starred_expanded?)}
              sessions={@sessions}
              added_at_label="Starred"
            >
              <:card_icon :let={{_info, idx}}>
                <span class="tooltip top" data-tooltip="Unstar">
                  <button
                    aria-label="unstar notebook"
                    phx-click={JS.push("unstar_notebook", value: %{idx: idx})}
                  >
                    <.remix_icon icon="star-fill" class="text-yellow-600" />
                  </button>
                </span>
              </:card_icon>
            </.live_component>
          <% end %>
        </div>

        <div id="running-sessions" class="py-20 mb-32" role="region" aria-label="running sessions">
          <.live_component
            module={LivebookWeb.HomeLive.SessionListComponent}
            id="session-list"
            sessions={@sessions}
            starred_notebooks={@starred_notebooks}
            memory={@memory}
          />
        </div>
      </div>
    </LayoutHelpers.layout>

    <.modal :if={@live_action == :import} id="import-modal" show width={:big} patch={@self_path}>
      <.live_component
        module={LivebookWeb.HomeLive.ImportComponent}
        id="import"
        tab={@tab}
        import_opts={@import_opts}
      />
    </.modal>
    """
  end

  defp update_notification(%{version: nil} = assigns), do: ~H""

  defp update_notification(assigns) do
    ~H"""
    <LayoutHelpers.topbar>
      <span>
        Livebook v<%= @version %> available!
        <%= if @instructions_url do %>
          Check out the news on
          <a
            class="font-medium border-b border-gray-900 hover:border-transparent"
            href="https://livebook.dev/"
            target="_blank"
          >
            livebook.dev
          </a>
          and follow the
          <a
            class="font-medium border-b border-gray-900 hover:border-transparent"
            href={@instructions_url}
            target="_blank"
          >
            update instructions
          </a>
        <% else %>
          Check out the news and installation steps on
          <a
            class="font-medium border-b border-gray-900 hover:border-transparent"
            href="https://livebook.dev/"
            target="_blank"
          >
            livebook.dev
          </a>
        <% end %>
        🚀
      </span>
    </LayoutHelpers.topbar>
    """
  end

  defp memory_notification(assigns) do
    ~H"""
    <LayoutHelpers.topbar :if={@app_service_url && @memory.free < 30_000_000} variant={:error}>
      <.remix_icon icon="alarm-warning-line" class="align-text-bottom mr-0.5" />
      Less than 30 MB of memory left, consider
      <a
        class="font-medium border-b border-gray-900 hover:border-transparent"
        href={@app_service_url}
        target="_blank"
      >
        adding more resources to the instance
      </a>
      or closing
      <a
        class="font-medium border-b border-gray-900 hover:border-transparent"
        href="#running-sessions"
      >
        running sessions
      </a>
    </LayoutHelpers.topbar>
    """
  end

  @impl true
  def handle_params(%{"session_id" => session_id}, _url, socket) do
    session = Enum.find(socket.assigns.sessions, &(&1.id == session_id))
    {:noreply, assign(socket, session: session)}
  end

  def handle_params(%{}, _url, socket) when socket.assigns.live_action == :public_new_notebook do
    {:noreply, create_session(socket, queue_setup: true)}
  end

  def handle_params(_params, _url, socket), do: {:noreply, socket}

  @impl true
  def handle_event("unstar_notebook", %{"idx" => idx}, socket) do
    on_confirm = fn socket ->
      %{file: file} = Enum.fetch!(socket.assigns.starred_notebooks, idx)
      Livebook.NotebookManager.remove_starred_notebook(file)
      socket
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Unstar notebook",
       description: "Once you unstar this notebook, you can always star it again.",
       confirm_text: "Unstar",
       opt_out_id: "unstar-notebook"
     )}
  end

  def handle_event("toggle_starred_expanded", %{}, socket) do
    {:noreply, update(socket, :starred_expanded?, &not/1)}
  end

  @impl true
  def handle_info({type, session} = event, socket)
      when type in [:session_created, :session_updated, :session_closed] and
             session.mode == :default do
    {:noreply, update(socket, :sessions, &update_session_list(&1, event))}
  end

  def handle_info({:memory_update, memory}, socket) do
    {:noreply, assign(socket, memory: memory)}
  end

  def handle_info({:starred_notebooks_updated, starred_notebooks}, socket) do
    {:noreply, assign(socket, starred_notebooks: starred_notebooks)}
  end

  def handle_info({:fork, file}, socket) do
    {:noreply, fork_notebook(socket, file)}
  end

  def handle_info({:open, file}, socket) do
    {:noreply, open_notebook(socket, file)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp visible_starred_notebooks(notebooks, starred_expanded?)
  defp visible_starred_notebooks(notebooks, true), do: notebooks
  defp visible_starred_notebooks(notebooks, false), do: Enum.take(notebooks, 6)
end

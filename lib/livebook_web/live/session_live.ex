defmodule LivebookWeb.SessionLive do
  use LivebookWeb, :live_view

  import LivebookWeb.UserHelpers
  import LivebookWeb.SessionHelpers
  import LivebookWeb.FileSystemHelpers
  import Livebook.Utils, only: [format_bytes: 1]

  alias Livebook.{Sessions, Session, Delta, Notebook, Runtime, LiveMarkdown}
  alias Livebook.Notebook.{Cell, ContentLoader}
  alias Livebook.JSInterop

  on_mount LivebookWeb.SidebarHook

  @impl true
  def mount(%{"id" => session_id}, _session, socket) do
    # We use the tracked sessions to locate the session pid, but then
    # we talk to the session process exclusively for getting all the information
    case Sessions.fetch_session(session_id) do
      {:ok, %{pid: session_pid}} ->
        {data, client_id} =
          if connected?(socket) do
            {data, client_id} =
              Session.register_client(session_pid, self(), socket.assigns.current_user)

            Session.subscribe(session_id)
            Livebook.NotebookManager.subscribe_starred_notebooks()

            {data, client_id}
          else
            data = Session.get_data(session_pid)
            {data, nil}
          end

        app =
          if slug = data.deployed_app_slug do
            {:ok, app} = Livebook.Apps.fetch_app(slug)

            if connected?(socket) do
              Livebook.App.subscribe(slug)
            end

            app
          end

        socket =
          if connected?(socket) do
            payload = %{
              clients:
                Enum.map(data.clients_map, fn {client_id, user_id} ->
                  client_info(client_id, data.users_map[user_id])
                end)
            }

            socket = push_event(socket, "session_init", payload)

            cells = for {cell, _} <- Notebook.cells_with_section(data.notebook), do: cell
            push_cell_editor_payloads(socket, data, cells)
          else
            socket
          end

        session = Session.get_by_pid(session_pid)
        platform = platform_from_socket(socket)

        socket =
          if data.mode == :app do
            put_flash(
              socket,
              :info,
              "This session is a deployed app. Any changes will be reflected live."
            )
          else
            socket
          end

        {:ok,
         socket
         |> assign(
           self_path: ~p"/sessions/#{session.id}",
           session: session,
           app: app,
           client_id: client_id,
           platform: platform,
           data_view: data_to_view(data),
           autofocus_cell_id: autofocus_cell_id(data.notebook),
           page_title: get_page_title(data.notebook.name),
           select_secret_ref: nil,
           select_secret_options: nil,
           allowed_uri_schemes: Livebook.Config.allowed_uri_schemes(),
           starred_files: Livebook.NotebookManager.starred_notebooks() |> starred_files()
         )
         |> assign_private(data: data)
         |> prune_outputs()
         |> prune_cell_sources()}

      {:error, :not_found} ->
        {:ok, redirect(socket, to: ~p"/")}

      {:error, :different_boot_id} ->
        {:ok,
         socket
         |> put_flash(
           :error,
           "Could not find notebook session because Livebook has rebooted. " <>
             "This may happen if Livebook runs out of memory while installing dependencies or executing code."
         )
         |> redirect(to: ~p"/")}
    end
  end

  # Puts the given assigns in `socket.private`,
  # to ensure they are not used for rendering.
  defp assign_private(socket, assigns) do
    Enum.reduce(assigns, socket, fn {key, value}, socket ->
      put_in(socket.private[key], value)
    end)
  end

  defp platform_from_socket(socket) do
    with user_agent when is_binary(user_agent) <- get_connect_info(socket, :user_agent) do
      platform_from_user_agent(user_agent)
    else
      _ -> nil
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      class="flex grow h-full"
      id={"session-#{@session.id}"}
      data-el-session
      phx-hook="Session"
      data-global-status={elem(@data_view.global_status, 0)}
      data-autofocus-cell-id={@autofocus_cell_id}
    >
      <nav
        class="w-16 flex flex-col items-center px-3 py-1 space-y-2 sm:space-y-3 sm:py-5 bg-gray-900"
        aria-label="sidebar"
        data-el-sidebar
      >
        <span>
          <.link navigate={~p"/"} aria-label="go to homepage">
            <img src={~p"/images/logo.png"} height="40" width="40" alt="" />
          </.link>
        </span>

        <.button_item
          icon="booklet-fill"
          label="Sections (ss)"
          button_attrs={["data-el-sections-list-toggle": true]}
        />
        <.button_item
          icon="group-fill"
          label="Connected users (su)"
          button_attrs={["data-el-clients-list-toggle": true]}
        />
        <.button_item
          icon="folder-open-fill"
          label="Files (sf)"
          button_attrs={["data-el-files-list-toggle": true]}
        />
        <.button_item
          icon="lock-password-line"
          label="Secrets (se)"
          button_attrs={["data-el-secrets-list-toggle": true]}
        />
        <div class="relative">
          <.button_item
            icon="rocket-line"
            label="App settings (sa)"
            button_attrs={["data-el-app-info-toggle": true]}
          />
          <div
            data-el-app-indicator
            class={[
              "absolute w-[12px] h-[12px] border-gray-900 border-2 rounded-full right-1.5 top-1.5 pointer-events-none",
              app_status_color(app_status(@app))
            ]}
          />
        </div>
        <.button_item
          icon="cpu-line"
          label="Runtime settings (sr)"
          button_attrs={["data-el-runtime-info-toggle": true]}
        />
        <.link_item
          icon="delete-bin-6-fill"
          label="Bin (sb)"
          path={~p"/sessions/#{@session.id}/bin"}
          active={@live_action == :bin}
          link_attrs={["data-btn-show-bin": true]}
        />

        <div class="grow"></div>

        <.link_item
          icon="keyboard-box-fill"
          label="Keyboard shortcuts (?)"
          path={~p"/sessions/#{@session.id}/shortcuts"}
          active={@live_action == :shortcuts}
          link_attrs={["data-btn-show-shortcuts": true]}
        />

        <span class="tooltip right distant" data-tooltip="User profile">
          <button
            class="text-gray-400 rounded-xl h-8 w-8 flex items-center justify-center mt-2 group"
            aria_label="user profile"
            phx-click={show_current_user_modal()}
          >
            <.user_avatar
              user={@current_user}
              class="w-8 h-8 group-hover:ring-white group-hover:ring-2"
              text_class="text-xs"
            />
          </button>
        </span>
      </nav>
      <div
        class="flex flex-col h-full w-full max-w-xs absolute z-30 top-0 left-[64px] overflow-y-auto shadow-xl md:static md:shadow-none bg-gray-50 border-r border-gray-100 px-6 pt-16 md:py-8"
        data-el-side-panel
      >
        <div class="flex grow" data-el-sections-list>
          <.sections_list data_view={@data_view} />
        </div>
        <div data-el-clients-list>
          <.clients_list data_view={@data_view} client_id={@client_id} />
        </div>
        <div data-el-files-list>
          <.live_component
            module={LivebookWeb.SessionLive.FilesListComponent}
            id="files-list"
            session={@session}
            file_entries={@data_view.file_entries}
            quarantine_file_entry_names={@data_view.quarantine_file_entry_names}
          />
        </div>
        <div data-el-secrets-list>
          <.live_component
            module={LivebookWeb.SessionLive.SecretsListComponent}
            id="secrets-list"
            session={@session}
            secrets={@data_view.secrets}
            hub_secrets={@data_view.hub_secrets}
            hub={@data_view.hub}
          />
        </div>
        <div data-el-app-info>
          <.live_component
            module={LivebookWeb.SessionLive.AppInfoComponent}
            id="app-info"
            session={@session}
            settings={@data_view.app_settings}
            app={@app}
            deployed_app_slug={@data_view.deployed_app_slug}
            any_session_secrets?={@data_view.any_session_secrets?}
          />
        </div>
        <div data-el-runtime-info>
          <.runtime_info data_view={@data_view} session={@session} />
        </div>
      </div>
      <div class="grow overflow-y-auto relative" data-el-notebook>
        <div data-el-js-view-iframes phx-update="ignore" id="js-view-iframes"></div>
        <LivebookWeb.SessionLive.IndicatorsComponent.render
          session_id={@session.id}
          file={@data_view.file}
          dirty={@data_view.dirty}
          persistence_warnings={@data_view.persistence_warnings}
          autosave_interval_s={@data_view.autosave_interval_s}
          runtime={@data_view.runtime}
          global_status={@data_view.global_status}
        />
        <div
          class="relative w-full max-w-screen-lg px-4 sm:pl-8 sm:pr-16 md:pl-16 pt-4 sm:py-5 mx-auto"
          data-el-notebook-content
        >
          <div class="pb-4 mb-2 border-b border-gray-200">
            <div class="flex flex-nowrap items-center gap-2">
              <div
                class="grow"
                data-el-notebook-headline
                data-focusable-id="notebook"
                id="notebook"
                phx-hook="Headline"
                data-on-value-change="set_notebook_name"
                data-metadata="notebook"
              >
                <h1
                  class="px-1 -ml-1.5 text-3xl font-semibold text-gray-800 border border-transparent rounded-lg whitespace-pre-wrap"
                  tabindex="0"
                  id="notebook-heading"
                  data-el-heading
                  spellcheck="false"
                  phx-no-format
                ><%= @data_view.notebook_name %></h1>
              </div>

              <.menu id="session-menu">
                <:toggle>
                  <button class="icon-button" aria-label="open notebook menu">
                    <.remix_icon icon="more-2-fill" class="text-xl" />
                  </button>
                </:toggle>
                <.menu_item>
                  <.link patch={~p"/sessions/#{@session.id}/export/livemd"} role="menuitem">
                    <.remix_icon icon="download-2-line" />
                    <span>Export</span>
                  </.link>
                </.menu_item>
                <.menu_item>
                  <button role="menuitem" phx-click="erase_outputs">
                    <.remix_icon icon="eraser-fill" />
                    <span>Erase outputs</span>
                  </button>
                </.menu_item>
                <.menu_item>
                  <button role="menuitem" phx-click="fork_session">
                    <.remix_icon icon="git-branch-line" />
                    <span>Fork</span>
                  </button>
                </.menu_item>
                <.menu_item>
                  <a role="menuitem" href={live_dashboard_process_path(@session.pid)} target="_blank">
                    <.remix_icon icon="dashboard-2-line" />
                    <span>See on Dashboard</span>
                  </a>
                </.menu_item>
                <.menu_item variant={:danger}>
                  <button role="menuitem" phx-click="close_session">
                    <.remix_icon icon="close-circle-line" />
                    <span>Close</span>
                  </button>
                </.menu_item>
              </.menu>
            </div>
            <div class="flex flex-nowrap place-content-between items-center gap-2">
              <.menu position={:bottom_left} id="notebook-hub-menu">
                <:toggle>
                  <div
                    class="inline-flex items-center group cursor-pointer gap-1 mt-1 text-sm text-gray-600 hover:text-gray-800 focus:text-gray-800"
                    aria-label={@data_view.hub.hub_name}
                  >
                    <span>in</span>
                    <span class="text-lg pl-1"><%= @data_view.hub.hub_emoji %></span>
                    <span><%= @data_view.hub.hub_name %></span>
                    <.remix_icon icon="arrow-down-s-line" class="invisible group-hover:visible" />
                  </div>
                </:toggle>
                <.menu_item :for={hub <- @saved_hubs}>
                  <button
                    id={"select-hub-#{hub.id}"}
                    phx-click={JS.push("select_hub", value: %{id: hub.id})}
                    aria-label={hub.name}
                    role="menuitem"
                  >
                    <%= hub.emoji %>
                    <span class="ml-2"><%= hub.name %></span>
                  </button>
                </.menu_item>
                <.menu_item>
                  <.link navigate={~p"/hub"} aria-label="Add Hub" role="menuitem">
                    <.remix_icon icon="add-line" class="align-middle mr-1" /> Add Hub
                  </.link>
                </.menu_item>
              </.menu>

              <div class="px-[1px]">
                <%= cond do %>
                  <% @data_view.file == nil -> %>
                    <span class="tooltip left" data-tooltip="Save this notebook before starring it">
                      <button class="icon-button" disabled>
                        <.remix_icon icon="star-line text-lg" />
                      </button>
                    </span>
                  <% @data_view.file in @starred_files -> %>
                    <span class="tooltip left" data-tooltip="Unstar notebook">
                      <button class="icon-button" phx-click="unstar_notebook">
                        <.remix_icon icon="star-fill text-lg text-yellow-600" />
                      </button>
                    </span>
                  <% true -> %>
                    <span class="tooltip left" data-tooltip="Star notebook">
                      <button class="icon-button" phx-click="star_notebook">
                        <.remix_icon icon="star-line text-lg" />
                      </button>
                    </span>
                <% end %>
              </div>
            </div>
          </div>
          <div>
            <.live_component
              module={LivebookWeb.SessionLive.CellComponent}
              id={@data_view.setup_cell_view.id}
              session_id={@session.id}
              session_pid={@session.pid}
              client_id={@client_id}
              runtime={@data_view.runtime}
              installing?={@data_view.installing?}
              allowed_uri_schemes={@allowed_uri_schemes}
              cell_view={@data_view.setup_cell_view}
            />
          </div>
          <div class="mt-8 flex flex-col w-full space-y-16" data-el-sections-container>
            <div :if={@data_view.section_views == []} class="flex justify-center">
              <button class="button-base button-small" phx-click="append_section">
                + Section
              </button>
            </div>
            <.live_component
              :for={{section_view, index} <- Enum.with_index(@data_view.section_views)}
              module={LivebookWeb.SessionLive.SectionComponent}
              id={section_view.id}
              index={index}
              session_id={@session.id}
              session_pid={@session.pid}
              client_id={@client_id}
              runtime={@data_view.runtime}
              smart_cell_definitions={@data_view.smart_cell_definitions}
              example_snippet_definitions={@data_view.example_snippet_definitions}
              installing?={@data_view.installing?}
              allowed_uri_schemes={@allowed_uri_schemes}
              section_view={section_view}
              default_language={@data_view.default_language}
            />
            <div style="height: 80vh"></div>
          </div>
        </div>
      </div>
    </div>

    <.current_user_modal current_user={@current_user} />

    <.modal
      :if={@live_action == :runtime_settings}
      id="runtime-settings-modal"
      show
      width={:big}
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.RuntimeComponent}
        id="runtime-settings"
        session={@session}
        runtime={@data_view.runtime}
      />
    </.modal>

    <.modal
      :if={@live_action == :file_settings}
      id="persistence-modal"
      show
      width={:big}
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.PersistenceComponent}
        id="persistence"
        session={@session}
        file={@data_view.file}
        persist_outputs={@data_view.persist_outputs}
        autosave_interval_s={@data_view.autosave_interval_s}
      />
    </.modal>

    <.modal
      :if={@live_action == :app_settings}
      id="app-settings-modal"
      show
      width={:medium}
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.AppSettingsComponent}
        id="app-settings"
        session={@session}
        settings={@data_view.app_settings}
      />
    </.modal>

    <.modal
      :if={@live_action == :add_file_entry}
      id="add-file-entry-modal"
      show
      width={:big}
      patch={@self_path}
    >
      <.add_file_entry_content session={@session} file_entries={@data_view.file_entries} tab={@tab} />
    </.modal>

    <.modal
      :if={@live_action == :shortcuts}
      id="shortcuts-modal"
      show
      width={:large}
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.ShortcutsComponent}
        id="shortcuts"
        platform={@platform}
      />
    </.modal>

    <.modal
      :if={@live_action == :cell_settings}
      id="cell-settings-modal"
      show
      width={:medium}
      patch={@self_path}
    >
      <.live_component
        module={settings_component_for(@cell)}
        id="cell-settings"
        session={@session}
        return_to={@self_path}
        cell={@cell}
      />
    </.modal>

    <.modal
      :if={@live_action == :insert_image}
      id="insert-image-modal"
      show
      width={:medium}
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.InsertImageComponent}
        id="insert-image"
        session={@session}
        return_to={@self_path}
        insert_image_metadata={@insert_image_metadata}
      />
    </.modal>

    <.modal
      :if={@live_action == :insert_file}
      id="insert-file-modal"
      show
      width={:medium}
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.InsertFileComponent}
        id="insert-file"
        session={@session}
        return_to={@self_path}
        insert_file_metadata={@insert_file_metadata}
      />
    </.modal>

    <.modal :if={@live_action == :bin} id="bin-modal" show width={:big} patch={@self_path}>
      <.live_component
        module={LivebookWeb.SessionLive.BinComponent}
        id="bin"
        session={@session}
        return_to={@self_path}
        bin_entries={@data_view.bin_entries}
      />
    </.modal>

    <.modal :if={@live_action == :export} id="export-modal" show width={:big} patch={@self_path}>
      <.live_component
        module={LivebookWeb.SessionLive.ExportComponent}
        id="export"
        session={@session}
        tab={@tab}
      />
    </.modal>

    <.modal
      :if={@live_action == :package_search}
      id="package-search-modal"
      show
      width={:medium}
      patch={@self_path}
    >
      <%= live_render(@socket, LivebookWeb.SessionLive.PackageSearchLive,
        id: "package-search",
        session: %{
          "session_pid" => @session.pid,
          "runtime" => @data_view.runtime,
          "return_to" => @self_path
        }
      ) %>
    </.modal>

    <.modal :if={@live_action == :secrets} id="secrets-modal" show width={:large} patch={@self_path}>
      <.live_component
        module={LivebookWeb.SessionLive.SecretsComponent}
        id="secrets"
        session={@session}
        secrets={@data_view.secrets}
        hub_secrets={@data_view.hub_secrets}
        hub={@data_view.hub}
        prefill_secret_name={@prefill_secret_name}
        select_secret_ref={@select_secret_ref}
        select_secret_options={@select_secret_options}
        return_to={@self_path}
      />
    </.modal>

    <.modal
      :if={@live_action == :custom_view_settings}
      id="custom-view-modal"
      show
      width={:medium}
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.CustomViewComponent}
        id="custom"
        return_to={@self_path}
        session={@session}
      />
    </.modal>
    """
  end

  defp button_item(assigns) do
    ~H"""
    <span class="tooltip right distant" data-tooltip={@label}>
      <button
        class="text-2xl text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center"
        aria-label={@label}
        {@button_attrs}
      >
        <.remix_icon icon={@icon} />
      </button>
    </span>
    """
  end

  defp link_item(assigns) do
    assigns = assign_new(assigns, :link_attrs, fn -> [] end)

    ~H"""
    <span class="tooltip right distant" data-tooltip={@label}>
      <.link
        patch={@path}
        class={[
          "text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center",
          @active && "text-gray-50 bg-gray-700"
        ]}
        aria-label={@label}
        {@link_attrs}
      >
        <.remix_icon icon={@icon} class="text-2xl" />
      </.link>
    </span>
    """
  end

  defp sections_list(assigns) do
    ~H"""
    <div class="flex flex-col grow">
      <h3 class="uppercase text-sm font-semibold text-gray-500">
        Sections
      </h3>
      <div class="flex flex-col mt-4 space-y-4">
        <div :for={section_item <- @data_view.sections_items} class="flex items-center">
          <button
            class="grow flex items-center text-gray-500 hover:text-gray-900 text-left"
            data-el-sections-list-item
            data-section-id={section_item.id}
          >
            <span class="flex items-center space-x-1">
              <span><%= section_item.name %></span>
              <% # Note: the container has overflow-y auto, so we cannot set overflow-x visible,
              # consequently we show the tooltip wrapped to a fixed number of characters %>
              <span
                :if={section_item.parent}
                {branching_tooltip_attrs(section_item.name, section_item.parent.name)}
              >
                <.remix_icon
                  icon="git-branch-line"
                  class="text-lg font-normal leading-none flip-horizontally"
                />
              </span>
            </span>
          </button>
          <.section_status
            status={elem(section_item.status, 0)}
            cell_id={elem(section_item.status, 1)}
          />
        </div>
      </div>
      <button
        class="inline-flex items-center justify-center p-8 py-1 mt-8 space-x-2 text-sm font-medium text-gray-500 border border-gray-400 border-dashed rounded-xl hover:bg-gray-100"
        phx-click="append_section"
      >
        <.remix_icon icon="add-line" class="text-lg align-center" />
        <span>New section</span>
      </button>
      <div class="grow"></div>
      <button
        class="inline-flex items-center justify-center p-8 py-1 mt-8 space-x-2 text-sm font-medium text-gray-500 border border-gray-400 border-dashed rounded-xl hover:bg-gray-100"
        data-el-section-toggle-collapse-all-button
      >
        <.remix_icon icon="split-cells-vertical" class="text-lg align-center" />
        <span>Expand/collapse all</span>
      </button>
    </div>
    """
  end

  defp clients_list(assigns) do
    ~H"""
    <div class="flex flex-col grow">
      <div class="flex items-center justify-between space-x-4 -mt-1">
        <h3 class="uppercase text-sm font-semibold text-gray-500">
          Users
        </h3>
        <span class="flex items-center px-2 py-1 space-x-2 text-sm bg-gray-200 rounded-lg">
          <span class="inline-flex w-3 h-3 bg-green-600 rounded-full"></span>
          <span><%= length(@data_view.clients) %> connected</span>
        </span>
      </div>
      <div class="flex flex-col mt-5 space-y-4">
        <div
          :for={{client_id, user} <- @data_view.clients}
          class="flex items-center justify-between space-x-2"
          id={"clients-list-item-#{client_id}"}
          data-el-clients-list-item
          data-client-id={client_id}
        >
          <button
            class="flex items-center space-x-2 text-gray-500 hover:text-gray-900 disabled:pointer-events-none"
            disabled={client_id == @client_id}
            data-el-client-link
          >
            <.user_avatar user={user} class="shrink-0 h-7 w-7" text_class="text-xs" />
            <span class="text-left"><%= user.name || "Anonymous" %></span>
          </button>
          <%= if client_id != @client_id do %>
            <span
              class="tooltip left"
              data-tooltip="Follow this user"
              data-el-client-follow-toggle
              data-meta="follow"
            >
              <button class="icon-button" aria-label="follow this user">
                <.remix_icon icon="pushpin-line" class="text-lg" />
              </button>
            </span>
            <span
              class="tooltip left"
              data-tooltip="Unfollow this user"
              data-el-client-follow-toggle
              data-meta="unfollow"
            >
              <button class="icon-button" aria-label="unfollow this user">
                <.remix_icon icon="pushpin-fill" class="text-lg" />
              </button>
            </span>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  defp runtime_info(assigns) do
    ~H"""
    <div class="flex flex-col grow">
      <div class="flex items-center justify-between">
        <h3 class="uppercase text-sm font-semibold text-gray-500">
          Runtime
        </h3>
        <.link patch={~p"/sessions/#{@session.id}/settings/runtime"} class="icon-button p-0">
          <.remix_icon icon="settings-3-line text-xl" />
        </.link>
      </div>
      <div class="flex flex-col mt-2 space-y-4">
        <div class="flex flex-col space-y-3">
          <.labeled_text
            :for={{label, value} <- Runtime.describe(@data_view.runtime)}
            label={label}
            one_line
          >
            <%= value %>
          </.labeled_text>
        </div>
        <div class="flex space-x-2">
          <%= if Runtime.connected?(@data_view.runtime) do %>
            <button class="button-base button-blue" phx-click="reconnect_runtime">
              <.remix_icon icon="wireless-charging-line" class="align-middle mr-1" />
              <span>Reconnect</span>
            </button>
            <button
              class="button-base button-outlined-red"
              type="button"
              phx-click="disconnect_runtime"
            >
              Disconnect
            </button>
          <% else %>
            <button class="button-base button-blue" phx-click="connect_runtime">
              <.remix_icon icon="wireless-charging-line" class="align-middle mr-1" />
              <span>Connect</span>
            </button>
            <.link
              patch={~p"/sessions/#{@session.id}/settings/runtime"}
              class="button-base button-outlined-gray bg-transparent"
            >
              Configure
            </.link>
          <% end %>
        </div>
        <%= if uses_memory?(@session.memory_usage) do %>
          <.memory_info memory_usage={@session.memory_usage} />
        <% else %>
          <div class="mb-1 text-sm text-gray-800 py-6 flex flex-col">
            <span class="w-full uppercase font-semibold text-gray-500">Memory</span>
            <p class="py-1">
              <%= format_bytes(@session.memory_usage.system.free) %> available out of <%= format_bytes(
                @session.memory_usage.system.total
              ) %>
            </p>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  defp memory_info(assigns) do
    assigns = assign(assigns, :runtime_memory, runtime_memory(assigns.memory_usage))

    ~H"""
    <div class="py-6 flex flex-col justify-center">
      <div class="mb-1 text-sm text-gray-800 flex flex-row justify-between">
        <span class="text-gray-500 font-semibold uppercase">Memory</span>
        <span class="text-right">
          <%= format_bytes(@memory_usage.system.free) %> available
        </span>
      </div>
      <div class="w-full h-8 flex flex-row py-1 gap-0.5">
        <div
          :for={{type, memory} <- @runtime_memory}
          class={["h-6", memory_color(type)]}
          style={"width: #{memory.percentage}%"}
        >
        </div>
      </div>
      <div class="flex flex-col py-1">
        <div :for={{type, memory} <- @runtime_memory} class="flex flex-row items-center">
          <span class={["w-4 h-4 mr-2 rounded", memory_color(type)]}></span>
          <span class="capitalize text-gray-700"><%= type %></span>
          <span class="text-gray-500 ml-auto"><%= memory.unit %></span>
        </div>
        <div class="flex rounded justify-center my-2 py-0.5 text-sm text-gray-800 bg-gray-200">
          Total: <%= format_bytes(@memory_usage.runtime.total) %>
        </div>
      </div>
    </div>
    """
  end

  defp section_status(%{status: :evaluating} = assigns) do
    ~H"""
    <button data-el-focus-cell-button data-target={@cell_id}>
      <.status_indicator variant={:progressing} />
    </button>
    """
  end

  defp section_status(%{status: :stale} = assigns) do
    ~H"""
    <button data-el-focus-cell-button data-target={@cell_id}>
      <.status_indicator variant={:warning} />
    </button>
    """
  end

  defp section_status(assigns), do: ~H""

  defp add_file_entry_content(assigns) do
    ~H"""
    <div class="p-6 max-w-4xl flex flex-col space-y-4">
      <h3 class="text-2xl font-semibold text-gray-800">
        Add file
      </h3>
      <div class="flex flex-col space-y-4">
        <div class="tabs">
          <.link
            patch={~p"/sessions/#{@session.id}/add-file/file"}
            class={["tab", @tab == "file" && "active"]}
          >
            <.remix_icon icon="file-3-line" class="align-middle" />
            <span class="font-medium">From file</span>
          </.link>
          <.link
            patch={~p"/sessions/#{@session.id}/add-file/url"}
            class={["tab", @tab == "url" && "active"]}
          >
            <.remix_icon icon="download-cloud-2-line" class="align-middle" />
            <span class="font-medium">From URL</span>
          </.link>
          <.link
            patch={~p"/sessions/#{@session.id}/add-file/upload"}
            class={["tab", @tab == "upload" && "active"]}
          >
            <.remix_icon icon="file-upload-line" class="align-middle" />
            <span class="font-medium">From upload</span>
          </.link>
          <.link
            patch={~p"/sessions/#{@session.id}/add-file/unlisted"}
            class={["tab", @tab == "unlisted" && "active"]}
          >
            <.remix_icon icon="folder-shared-line" class="align-middle" />
            <span class="font-medium">From unlisted</span>
          </.link>
          <div class="grow tab"></div>
        </div>
        <.live_component
          :if={@tab == "file"}
          module={LivebookWeb.SessionLive.AddFileEntryFileComponent}
          id="add-file-entry-from-file"
          session={@session}
        />
        <.live_component
          :if={@tab == "url"}
          module={LivebookWeb.SessionLive.AddFileEntryUrlComponent}
          id="add-file-entry-from-url"
          session={@session}
        />
        <.live_component
          :if={@tab == "upload"}
          module={LivebookWeb.SessionLive.AddFileEntryUploadComponent}
          id="add-file-entry-from-upload"
          session={@session}
        />
        <.live_component
          :if={@tab == "unlisted"}
          module={LivebookWeb.SessionLive.AddFileEntryUnlistedComponent}
          id="add-file-entry-from-unlisted"
          session={@session}
          file_entries={@file_entries}
        />
      </div>
    </div>
    """
  end

  defp settings_component_for(%Cell.Code{}),
    do: LivebookWeb.SessionLive.CodeCellSettingsComponent

  defp branching_tooltip_attrs(name, parent_name) do
    direction = if String.length(name) >= 16, do: "left", else: "right"

    wrapped_name = Livebook.Utils.wrap_line("”" <> parent_name <> "”", 16)
    label = "Branches from\n#{wrapped_name}"

    [class: "tooltip #{direction}", data_tooltip: label]
  end

  @impl true
  def handle_params(%{"cell_id" => cell_id}, _url, socket)
      when socket.assigns.live_action == :cell_settings do
    {:ok, cell, _} = Notebook.fetch_cell_and_section(socket.private.data.notebook, cell_id)
    {:noreply, assign(socket, cell: cell)}
  end

  def handle_params(%{}, _url, socket)
      when socket.assigns.live_action == :insert_image and
             not is_map_key(socket.assigns, :insert_image_metadata) do
    {:noreply, redirect_to_self(socket)}
  end

  def handle_params(%{}, _url, socket)
      when socket.assigns.live_action == :insert_file and
             not is_map_key(socket.assigns, :insert_file_metadata) do
    {:noreply, redirect_to_self(socket)}
  end

  def handle_params(%{"path_parts" => path_parts}, requested_url, socket)
      when socket.assigns.live_action == :catch_all do
    path_parts =
      Enum.map(path_parts, fn
        "__parent__" -> ".."
        part -> part
      end)

    path = Path.join(path_parts)
    {:noreply, handle_relative_path(socket, path, requested_url)}
  end

  def handle_params(%{"tab" => tab}, _url, socket) when socket.assigns.live_action == :export do
    {:noreply, assign(socket, tab: tab)}
  end

  def handle_params(%{"tab" => tab} = params, _url, socket)
      when socket.assigns.live_action == :add_file_entry do
    file_drop_metadata =
      if(params["file_drop"] == "true", do: socket.assigns[:file_drop_metadata])

    {:noreply, assign(socket, tab: tab, file_drop_metadata: file_drop_metadata)}
  end

  def handle_params(params, _url, socket)
      when socket.assigns.live_action == :secrets do
    socket =
      if params["preselect_name"] do
        assign(socket, prefill_secret_name: params["preselect_name"])
      else
        # Erase any previously stored reference
        assign(socket,
          prefill_secret_name: params["secret_name"],
          select_secret_ref: nil,
          select_secret_options: nil
        )
      end

    {:noreply, socket}
  end

  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("append_section", %{}, socket) do
    idx = length(socket.private.data.notebook.sections)
    Session.insert_section(socket.assigns.session.pid, idx)

    {:noreply, socket}
  end

  def handle_event("insert_section_below", params, socket) do
    with {:ok, section, index} <-
           section_with_next_index(
             socket.private.data.notebook,
             params["section_id"],
             params["cell_id"]
           ) do
      Session.insert_section_into(socket.assigns.session.pid, section.id, index)
    end

    {:noreply, socket}
  end

  def handle_event(
        "set_section_parent",
        %{"section_id" => section_id, "parent_id" => parent_id},
        socket
      ) do
    Session.set_section_parent(socket.assigns.session.pid, section_id, parent_id)

    {:noreply, socket}
  end

  def handle_event("unset_section_parent", %{"section_id" => section_id}, socket) do
    Session.unset_section_parent(socket.assigns.session.pid, section_id)

    {:noreply, socket}
  end

  def handle_event("insert_cell_below", params, socket) do
    {:noreply, insert_cell_below(socket, params)}
  end

  def handle_event("insert_example_snippet_below", params, socket) do
    data = socket.private.data
    %{"section_id" => section_id, "cell_id" => cell_id} = params

    if Livebook.Runtime.connected?(socket.private.data.runtime) do
      case example_snippet_definition_by_name(data, params["definition_name"]) do
        {:ok, definition} ->
          variant = Enum.fetch!(definition.variants, params["variant_idx"])

          socket =
            ensure_packages_then(socket, variant.packages, definition.name, "block", fn socket ->
              with {:ok, section, index} <-
                     section_with_next_index(socket.private.data.notebook, section_id, cell_id) do
                attrs = %{source: variant.source}
                Session.insert_cell(socket.assigns.session.pid, section.id, index, :code, attrs)
                {:ok, socket}
              end
            end)

          {:noreply, socket}

        _ ->
          {:noreply, socket}
      end
    else
      reason = "To insert this block, you need a connected runtime."
      {:noreply, confirm_setup_default_runtime(socket, reason)}
    end
  end

  def handle_event("insert_smart_cell_below", params, socket) do
    data = socket.private.data
    %{"section_id" => section_id, "cell_id" => cell_id} = params

    case smart_cell_definition_by_kind(data, params["kind"]) do
      {:ok, definition} ->
        preset =
          if preset_idx = params["preset_idx"] do
            Enum.at(definition.requirement_presets, preset_idx)
          end

        packages = if(preset, do: preset.packages, else: [])

        socket =
          ensure_packages_then(socket, packages, definition.name, "smart cell", fn socket ->
            with {:ok, section, index} <-
                   section_with_next_index(socket.private.data.notebook, section_id, cell_id) do
              attrs = %{kind: definition.kind}
              Session.insert_cell(socket.assigns.session.pid, section.id, index, :smart, attrs)
              {:ok, socket}
            end
          end)

        {:noreply, socket}

      _ ->
        {:noreply, socket}
    end
  end

  def handle_event("set_default_language", %{"language" => language} = params, socket)
      when language in ["elixir", "erlang"] do
    language = String.to_atom(language)
    Session.set_notebook_attributes(socket.assigns.session.pid, %{default_language: language})
    {:noreply, insert_cell_below(socket, params)}
  end

  def handle_event("delete_cell", %{"cell_id" => cell_id}, socket) do
    on_confirm = fn socket ->
      Session.delete_cell(socket.assigns.session.pid, cell_id)
      socket
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Delete cell",
       description: "Once you delete this cell, it will be moved to the bin.",
       confirm_text: "Delete",
       confirm_icon: "delete-bin-6-line",
       opt_out_id: "delete-cell"
     )}
  end

  def handle_event("set_notebook_name", %{"value" => name}, socket) do
    name = normalize_name(name)
    Session.set_notebook_name(socket.assigns.session.pid, name)

    {:noreply, socket}
  end

  def handle_event("set_section_name", %{"metadata" => section_id, "value" => name}, socket) do
    name = normalize_name(name)
    Session.set_section_name(socket.assigns.session.pid, section_id, name)

    {:noreply, socket}
  end

  def handle_event(
        "apply_cell_delta",
        %{"cell_id" => cell_id, "tag" => tag, "delta" => delta, "revision" => revision},
        socket
      ) do
    tag = String.to_atom(tag)
    delta = Delta.from_compressed(delta)
    Session.apply_cell_delta(socket.assigns.session.pid, cell_id, tag, delta, revision)

    {:noreply, socket}
  end

  def handle_event(
        "report_cell_revision",
        %{"cell_id" => cell_id, "tag" => tag, "revision" => revision},
        socket
      ) do
    tag = String.to_atom(tag)
    Session.report_cell_revision(socket.assigns.session.pid, cell_id, tag, revision)

    {:noreply, socket}
  end

  def handle_event("move_cell", %{"cell_id" => cell_id, "offset" => offset}, socket) do
    offset = ensure_integer(offset)
    Session.move_cell(socket.assigns.session.pid, cell_id, offset)

    {:noreply, socket}
  end

  def handle_event("move_section", %{"section_id" => section_id, "offset" => offset}, socket) do
    offset = ensure_integer(offset)
    Session.move_section(socket.assigns.session.pid, section_id, offset)

    {:noreply, socket}
  end

  def handle_event("delete_section", %{"section_id" => section_id}, socket) do
    socket =
      case Notebook.fetch_section(socket.private.data.notebook, section_id) do
        {:ok, %{cells: []} = section} ->
          Session.delete_section(socket.assigns.session.pid, section.id, true)
          socket

        {:ok, section} ->
          section_id = section.id
          first_section? = hd(socket.private.data.notebook.sections).id == section.id

          on_confirm = fn socket, %{"delete_cells" => delete_cells} ->
            Livebook.Session.delete_section(socket.assigns.session.pid, section_id, delete_cells)
            socket
          end

          assigns = %{section_name: section.name}

          description = ~H"""
          Are you sure you want to delete this section - <span class="font-semibold">“<%= @section_name %>”</span>?
          """

          confirm(socket, on_confirm,
            title: "Delete section",
            description: description,
            confirm_text: "Delete",
            confirm_icon: "delete-bin-6-line",
            options: [
              %{
                name: "delete_cells",
                label: "Delete all cells in this section",
                default: first_section?,
                disabled: first_section?
              }
            ]
          )

        :error ->
          socket
      end

    {:noreply, socket}
  end

  def handle_event("recover_smart_cell", %{"cell_id" => cell_id}, socket) do
    Session.recover_smart_cell(socket.assigns.session.pid, cell_id)

    {:noreply, socket}
  end

  def handle_event("convert_smart_cell", %{"cell_id" => cell_id}, socket) do
    on_confirm = fn socket ->
      Session.convert_smart_cell(socket.assigns.session.pid, cell_id)
      socket
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Convert cell",
       description:
         "Once you convert this Smart cell to a Code cell, the Smart cell will be moved to the bin.",
       confirm_text: "Convert",
       confirm_icon: "arrow-up-down-line",
       opt_out_id: "convert-smart-cell"
     )}
  end

  def handle_event("add_form_cell_dependencies", %{}, socket) do
    Session.add_dependencies(socket.assigns.session.pid, [%{dep: {:kino, "~> 0.8.1"}, config: []}])

    {status, socket} = maybe_reconnect_runtime(socket)

    if status == :ok do
      Session.queue_cell_evaluation(socket.assigns.session.pid, Cell.setup_cell_id())
    end

    {:noreply, socket}
  end

  def handle_event("queue_cell_evaluation", %{"cell_id" => cell_id} = params, socket) do
    data = socket.private.data

    {status, socket} =
      with {:ok, cell, _section} <- Notebook.fetch_cell_and_section(data.notebook, cell_id),
           true <- Cell.setup?(cell),
           false <- data.cell_infos[cell.id].eval.validity == :fresh do
        maybe_reconnect_runtime(socket)
      else
        _ -> {:ok, socket}
      end

    if params["disable_dependencies_cache"] do
      Session.disable_dependencies_cache(socket.assigns.session.pid)
    end

    if status == :ok do
      Session.queue_cell_evaluation(socket.assigns.session.pid, cell_id)
    end

    {:noreply, socket}
  end

  @impl true
  def handle_event("queue_interrupted_cell_evaluation", %{"cell_id" => cell_id}, socket) do
    data = socket.private.data

    with {:ok, cell, _section} <- Notebook.fetch_cell_and_section(data.notebook, cell_id),
         true <- data.cell_infos[cell.id].eval.interrupted do
      Session.queue_cell_evaluation(socket.assigns.session.pid, cell_id)
    end

    {:noreply, socket}
  end

  def handle_event("queue_section_evaluation", %{"section_id" => section_id}, socket) do
    Session.queue_section_evaluation(socket.assigns.session.pid, section_id)

    {:noreply, socket}
  end

  def handle_event("queue_full_evaluation", %{"forced_cell_ids" => forced_cell_ids}, socket) do
    Session.queue_full_evaluation(socket.assigns.session.pid, forced_cell_ids)

    {:noreply, socket}
  end

  def handle_event("cancel_cell_evaluation", %{"cell_id" => cell_id}, socket) do
    Session.cancel_cell_evaluation(socket.assigns.session.pid, cell_id)

    {:noreply, socket}
  end

  def handle_event(
        "set_reevaluate_automatically",
        %{"value" => value, "cell_id" => cell_id},
        socket
      ) do
    Session.set_cell_attributes(socket.assigns.session.pid, cell_id, %{
      reevaluate_automatically: value
    })

    {:noreply, socket}
  end

  def handle_event("save", %{}, socket) do
    if socket.private.data.file do
      Session.save(socket.assigns.session.pid)
      {:noreply, socket}
    else
      {:noreply, push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}/settings/file")}
    end
  end

  def handle_event("reconnect_runtime", %{}, socket) do
    {_, socket} = maybe_reconnect_runtime(socket)
    {:noreply, socket}
  end

  def handle_event("connect_runtime", %{}, socket) do
    {_, socket} = connect_runtime(socket)
    {:noreply, socket}
  end

  def handle_event("setup_default_runtime", %{"reason" => reason}, socket) do
    {:noreply, confirm_setup_default_runtime(socket, reason)}
  end

  def handle_event("disconnect_runtime", %{}, socket) do
    Session.disconnect_runtime(socket.assigns.session.pid)
    {:noreply, socket}
  end

  def handle_event("deploy_app", %{}, socket) do
    on_confirm = fn socket ->
      Livebook.Session.deploy_app(socket.assigns.session.pid)
      socket
    end

    data = socket.private.data
    slug = data.notebook.app_settings.slug
    slug_taken? = slug != data.deployed_app_slug and Livebook.Apps.exists?(slug)

    socket =
      if slug_taken? do
        confirm(socket, on_confirm,
          title: "Deploy app",
          description:
            "An app with this slug already exists, do you want to deploy a new version?",
          confirm_text: "Replace"
        )
      else
        on_confirm.(socket)
      end

    {:noreply, socket}
  end

  def handle_event("intellisense_request", %{"cell_id" => cell_id} = params, socket) do
    request =
      case params do
        %{"type" => "completion", "hint" => hint} ->
          {:completion, hint}

        %{"type" => "details", "line" => line, "column" => column} ->
          column = JSInterop.js_column_to_elixir(column, line)
          {:details, line, column}

        %{"type" => "signature", "hint" => hint} ->
          {:signature, hint}

        %{"type" => "format", "code" => code} ->
          {:format, code}
      end

    data = socket.private.data

    with {:ok, cell, _section} <- Notebook.fetch_cell_and_section(data.notebook, cell_id) do
      if Runtime.connected?(data.runtime) do
        parent_locators = Session.parent_locators_for_cell(data, cell)
        ref = Runtime.handle_intellisense(data.runtime, self(), request, parent_locators)

        {:reply, %{"ref" => inspect(ref)}, socket}
      else
        info =
          cond do
            params["type"] == "completion" and not params["editor_auto_completion"] ->
              "You need to start a runtime (or evaluate a cell) for code completion"

            params["type"] == "format" ->
              "You need to start a runtime (or evaluate a cell) to enable code formatting"

            true ->
              nil
          end

        socket = if info, do: put_flash(socket, :info, info), else: socket
        {:reply, %{"ref" => nil}, socket}
      end
    else
      _ -> {:noreply, socket}
    end
  end

  def handle_event("fork_session", %{}, socket) do
    %{pid: pid, files_dir: files_dir} = socket.assigns.session
    # Fetch the data, as we don't keep cells' source in the state
    data = Session.get_data(pid)
    notebook = Notebook.forked(data.notebook)
    {:noreply, create_session(socket, notebook: notebook, files_source: {:dir, files_dir})}
  end

  def handle_event("close_session", %{}, socket) do
    {:noreply, confirm_close_session(socket, socket.assigns.session, redirect_to: ~p"/")}
  end

  def handle_event("star_notebook", %{}, socket) do
    data = socket.private.data
    Livebook.NotebookManager.add_starred_notebook(data.file, data.notebook.name)
    {:noreply, socket}
  end

  def handle_event("unstar_notebook", %{}, socket) do
    Livebook.NotebookManager.remove_starred_notebook(socket.private.data.file)
    {:noreply, socket}
  end

  def handle_event("erase_outputs", %{}, socket) do
    Session.erase_outputs(socket.assigns.session.pid)
    {:noreply, socket}
  end

  def handle_event("location_report", report, socket) do
    Phoenix.PubSub.broadcast_from(
      Livebook.PubSub,
      self(),
      "sessions:#{socket.assigns.session.id}",
      {:location_report, socket.assigns.client_id, report}
    )

    {:noreply, socket}
  end

  def handle_event(
        "select_secret",
        %{
          "js_view_ref" => select_secret_ref,
          "preselect_name" => preselect_name,
          "options" => select_secret_options
        },
        socket
      ) do
    socket =
      assign(socket,
        select_secret_ref: select_secret_ref,
        select_secret_options: select_secret_options
      )

    {:noreply,
     push_patch(socket,
       to: ~p"/sessions/#{socket.assigns.session.id}/secrets?preselect_name=#{preselect_name}"
     )}
  end

  def handle_event("select_hub", %{"id" => id}, socket) do
    Session.set_notebook_hub(socket.assigns.session.pid, id)

    {:noreply, socket}
  end

  def handle_event("review_file_entry_access", %{"name" => name}, socket) do
    if file_entry = find_file_entry(socket, name) do
      on_confirm = fn socket ->
        Session.allow_file_entry(socket.assigns.session.pid, file_entry.name)
        socket
      end

      assigns = %{name: file_entry.name, file: file_entry.file}

      description = ~H"""
      <div>
        File <span class="font-semibold">“<%= @name %>“</span>
        points to an absolute path, do you want the notebook to access it?
      </div>
      <div class="mt-4 flex flex-col gap-2 border border-gray-200 rounded-lg p-4">
        <.labeled_text label="Path"><%= @file.path %></.labeled_text>
        <.labeled_text label="File system"><%= file_system_label(@file.file_system) %></.labeled_text>
      </div>
      """

      {:noreply,
       confirm(socket, on_confirm,
         title: "Review access",
         description: description,
         confirm_text: "Allow access",
         confirm_icon: "shield-check-line",
         danger: false
       )}
    else
      {:noreply, socket}
    end
  end

  def handle_event("insert_image", params, socket) do
    {:noreply,
     socket
     |> assign(
       insert_image_metadata: %{section_id: params["section_id"], cell_id: params["cell_id"]}
     )
     |> push_patch(to: ~p"/sessions/#{socket.assigns.session.id}/insert-image")}
  end

  def handle_event(
        "insert_file",
        %{"section_id" => section_id, "cell_id" => cell_id, "file_entry_name" => file_entry_name},
        socket
      ) do
    if file_entry = find_file_entry(socket, file_entry_name) do
      if Livebook.Runtime.connected?(socket.private.data.runtime) do
        {:noreply,
         socket
         |> assign(
           insert_file_metadata: %{
             section_id: section_id,
             cell_id: cell_id,
             file_entry: file_entry,
             handlers: handlers_for_file_entry(file_entry, socket.private.data.runtime)
           }
         )
         |> push_patch(to: ~p"/sessions/#{socket.assigns.session.id}/insert-file")}
      else
        reason = "To see the available options, you need a connected runtime."
        {:noreply, confirm_setup_default_runtime(socket, reason)}
      end
    else
      {:noreply, socket}
    end
  end

  def handle_event("insert_file_action", %{"idx" => idx}, socket) do
    %{section_id: section_id, cell_id: cell_id, file_entry: file_entry, handlers: handlers} =
      socket.assigns.insert_file_metadata

    handler = Enum.fetch!(handlers, idx)
    source = String.replace(handler.definition.source, "{{NAME}}", file_entry.name)

    socket =
      socket
      |> redirect_to_self()
      |> ensure_packages_then(
        handler.definition.packages,
        handler.definition.description,
        "action",
        fn socket ->
          with {:ok, section, index} <-
                 section_with_next_index(socket.private.data.notebook, section_id, cell_id) do
            attrs = %{source: source}

            Session.insert_cell(
              socket.assigns.session.pid,
              section.id,
              index,
              handler.cell_type,
              attrs
            )

            {:ok, socket}
          end
        end
      )

    {:noreply, socket}
  end

  def handle_event(
        "handle_file_drop",
        %{"section_id" => section_id, "cell_id" => cell_id},
        socket
      ) do
    if Livebook.Runtime.connected?(socket.private.data.runtime) do
      {:noreply,
       socket
       |> assign(file_drop_metadata: %{section_id: section_id, cell_id: cell_id})
       |> push_patch(
         to: ~p"/sessions/#{socket.assigns.session.id}/add-file/upload?file_drop=true"
       )
       |> push_event("finish_file_drop", %{})}
    else
      reason = "To see the available options, you need a connected runtime."
      {:noreply, confirm_setup_default_runtime(socket, reason)}
    end
  end

  def handle_event("handle_file_drop", %{}, socket) do
    {:noreply,
     socket
     |> assign(file_drop_metadata: nil)
     |> push_patch(to: ~p"/sessions/#{socket.assigns.session.id}/add-file/upload?file_drop=true")
     |> push_event("finish_file_drop", %{})}
  end

  def handle_event("open_custom_view_settings", %{}, socket) do
    {:noreply,
     push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}/settings/custom-view")}
  end

  @impl true
  def handle_info({:operation, operation}, socket) do
    {:noreply, handle_operation(socket, operation)}
  end

  def handle_info({:error, error}, socket) do
    message = error |> to_string() |> upcase_first()
    socket = put_flash(socket, :error, message)

    # If there is an immediate patch, we apply it to keep the flash
    socket =
      receive do
        {:push_patch, to} -> push_patch(socket, to: to)
      after
        0 -> socket
      end

    {:noreply, socket}
  end

  def handle_info({:hydrate_bin_entries, hydrated_entries}, socket) do
    hydrated_entries_map = Map.new(hydrated_entries, fn entry -> {entry.cell.id, entry} end)

    data =
      Map.update!(socket.private.data, :bin_entries, fn bin_entries ->
        Enum.map(bin_entries, fn entry ->
          case Map.fetch(hydrated_entries_map, entry.cell.id) do
            {:ok, hydrated_entry} -> hydrated_entry
            :error -> entry
          end
        end)
      end)

    {:noreply,
     socket
     |> assign_private(data: data)
     |> assign(data_view: data_to_view(data))}
  end

  def handle_info({:hydrate_cell_source_digest, cell_id, tag, digest}, socket) do
    data = socket.private.data
    data = put_in(data.cell_infos[cell_id].sources[tag].digest, digest)
    # We don't recompute data_view, because for the cell indicator we
    # still compute the digest on the client side
    {:noreply, assign_private(socket, data: data)}
  end

  def handle_info({:session_updated, session}, socket) do
    {:noreply, assign(socket, :session, session)}
  end

  def handle_info(:session_closed, socket) do
    {:noreply,
     socket
     |> put_flash(:info, "Session has been closed")
     |> push_navigate(to: ~p"/")}
  end

  def handle_info({:runtime_intellisense_response, ref, request, response}, socket) do
    response = process_intellisense_response(response, request)
    payload = %{"ref" => inspect(ref), "response" => response}
    {:noreply, push_event(socket, "intellisense_response", payload)}
  end

  def handle_info({:location_report, client_id, report}, socket) do
    report = Map.put(report, :client_id, client_id)
    {:noreply, push_event(socket, "location_report", report)}
  end

  def handle_info({:set_input_values, values, local}, socket) do
    if local do
      socket =
        Enum.reduce(values, socket, fn {input_id, value}, socket ->
          operation = {:set_input_value, socket.assigns.client_id, input_id, value}
          handle_operation(socket, operation)
        end)

      {:noreply, socket}
    else
      for {input_id, value} <- values do
        Session.set_input_value(socket.assigns.session.pid, input_id, value)
      end

      {:noreply, socket}
    end
  end

  def handle_info({:queue_bound_cells_evaluation, input_id}, socket) do
    Session.queue_bound_cells_evaluation(socket.assigns.session.pid, input_id)

    {:noreply, socket}
  end

  def handle_info({:insert_image_complete, metadata, url}, socket) do
    params = %{
      "type" => "image",
      "section_id" => metadata.section_id,
      "cell_id" => metadata.cell_id,
      "url" => url
    }

    {:noreply, insert_cell_below(socket, params)}
  end

  def handle_info({:file_entry_uploaded, file_entry}, socket) do
    case socket.assigns.file_drop_metadata do
      %{section_id: section_id, cell_id: cell_id} ->
        {:noreply,
         socket
         |> assign(
           insert_file_metadata: %{
             section_id: section_id,
             cell_id: cell_id,
             file_entry: file_entry,
             handlers: handlers_for_file_entry(file_entry, socket.private.data.runtime)
           }
         )
         |> push_patch(to: ~p"/sessions/#{socket.assigns.session.id}/insert-file")}

      nil ->
        {:noreply, socket}
    end
  end

  def handle_info({:push_patch, to}, socket) do
    {:noreply, push_patch(socket, to: to)}
  end

  def handle_info({:put_flash, kind, message}, socket) do
    {:noreply, put_flash(socket, kind, message)}
  end

  def handle_info({:starred_notebooks_updated, starred_notebooks}, socket) do
    {:noreply, assign(socket, starred_files: starred_files(starred_notebooks))}
  end

  def handle_info({:app_updated, app}, socket) when socket.assigns.app != nil do
    {:noreply, assign(socket, :app, app)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp handle_relative_path(socket, path, requested_url) do
    cond do
      String.ends_with?(path, LiveMarkdown.extension()) ->
        handle_relative_notebook_path(socket, path, requested_url)

      true ->
        socket
        |> put_flash(
          :error,
          "Got unrecognised session path: #{path}\nIf you want to link another notebook, make sure to include the .livemd extension"
        )
        |> redirect_to_self()
    end
  end

  defp handle_relative_notebook_path(socket, relative_path, requested_url) do
    resolution_location = location(socket.private.data)

    case resolution_location do
      nil ->
        socket
        |> put_flash(
          :info,
          "Cannot resolve notebook path #{relative_path}, because the current notebook has no location"
        )
        |> redirect_to_self()

      resolution_location ->
        origin = ContentLoader.resolve_location(resolution_location, relative_path)
        fallback_locations = fallback_relative_locations(resolution_location, relative_path)

        case session_id_by_location(origin) do
          {:ok, session_id} ->
            redirect_path = session_path(session_id, url_hash: get_url_hash(requested_url))
            push_navigate(socket, to: redirect_path)

          {:error, :none} ->
            open_notebook(socket, origin, fallback_locations, requested_url)

          {:error, :many} ->
            origin_str =
              case origin do
                {:url, url} -> url
                {:file, file} -> file.path
              end

            socket
            |> put_flash(
              :error,
              "Cannot navigate, because multiple sessions were found for #{origin_str}"
            )
            |> redirect_to_self()
        end
    end
  end

  defp location(data)
  defp location(%{file: file}) when is_map(file), do: {:file, file}
  defp location(%{origin: origin}), do: origin

  defp get_url_hash(requested_url) do
    case String.split(requested_url, "#") do
      [_, url_hash] -> url_hash
      _ -> nil
    end
  end

  defp open_notebook(socket, origin, fallback_locations, requested_url) do
    case fetch_content_with_fallbacks(origin, fallback_locations) do
      {:ok, content} ->
        {notebook, messages} = Livebook.LiveMarkdown.notebook_from_livemd(content)

        # If the current session has no file, fork the notebook
        fork? = socket.private.data.file == nil
        {file, notebook} = file_and_notebook(fork?, origin, notebook)
        url_hash = get_url_hash(requested_url)

        socket
        |> put_import_warnings(messages)
        |> create_session(
          notebook: notebook,
          origin: origin,
          file: file,
          url_hash: url_hash
        )

      {:error, message} ->
        socket
        |> put_flash(:error, "Cannot navigate, " <> message)
        |> redirect_to_self()
    end
  end

  def fallback_relative_locations({:file, _}, _relative_path), do: []

  def fallback_relative_locations(resolution_location, relative_path) do
    # Other locations to check in case the relative location doesn't
    # exist. For example, in ExDoc all pages (including notebooks) are
    # flat, regardless of how they are structured in the file system

    name = relative_path |> String.split("/") |> Enum.at(-1)
    flat_location = ContentLoader.resolve_location(resolution_location, name)
    [flat_location]
  end

  defp fetch_content_with_fallbacks(location, fallbacks) do
    case ContentLoader.fetch_content_from_location(location) do
      {:ok, content} ->
        {:ok, content}

      error ->
        fallbacks
        |> Enum.reject(&(&1 == location))
        |> Enum.find_value(error, fn fallback ->
          with {:error, _} <- ContentLoader.fetch_content_from_location(fallback), do: nil
        end)
    end
  end

  defp file_and_notebook(fork?, origin, notebook)
  defp file_and_notebook(false, {:file, file}, notebook), do: {file, notebook}
  defp file_and_notebook(true, {:file, _file}, notebook), do: {nil, Notebook.forked(notebook)}
  defp file_and_notebook(_fork?, _origin, notebook), do: {nil, notebook}

  defp session_id_by_location(location) do
    sessions = Sessions.list_sessions()

    session_with_file =
      Enum.find(sessions, fn session ->
        session.file && {:file, session.file} == location
      end)

    # A session associated with the given file takes
    # precedence over sessions originating from this file
    if session_with_file do
      {:ok, session_with_file.id}
    else
      sessions
      |> Enum.filter(fn session -> session.origin == location end)
      |> case do
        [session] -> {:ok, session.id}
        [] -> {:error, :none}
        _ -> {:error, :many}
      end
    end
  end

  defp redirect_to_self(socket) do
    push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}")
  end

  defp handle_operation(socket, operation) do
    case Session.Data.apply_operation(socket.private.data, operation) do
      {:ok, data, actions} ->
        socket
        |> assign_private(data: data)
        |> assign(
          data_view:
            update_data_view(socket.assigns.data_view, socket.private.data, data, operation)
        )
        |> after_operation(socket, operation)
        |> handle_actions(actions)

      :error ->
        socket
    end
  end

  defp after_operation(socket, _prev_socket, {:client_join, client_id, user}) do
    push_event(socket, "client_joined", %{client: client_info(client_id, user)})
  end

  defp after_operation(socket, _prev_socket, {:client_leave, client_id}) do
    push_event(socket, "client_left", %{client_id: client_id})
  end

  defp after_operation(socket, _prev_socket, {:update_user, _client_id, user}) do
    updated_clients =
      socket.private.data.clients_map
      |> Enum.filter(fn {_client_id, user_id} -> user_id == user.id end)
      |> Enum.map(fn {client_id, _user_id} -> client_info(client_id, user) end)

    push_event(socket, "clients_updated", %{clients: updated_clients})
  end

  defp after_operation(socket, _prev_socket, {:set_notebook_name, _client_id, name}) do
    assign(socket, page_title: get_page_title(name))
  end

  defp after_operation(socket, _prev_socket, {:insert_section, client_id, _index, section_id}) do
    if client_id == socket.assigns.client_id do
      push_event(socket, "section_inserted", %{section_id: section_id})
    else
      socket
    end
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:insert_section_into, client_id, _section_id, _index, section_id}
       ) do
    if client_id == socket.assigns.client_id do
      push_event(socket, "section_inserted", %{section_id: section_id})
    else
      socket
    end
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:delete_section, _client_id, section_id, _delete_cells}
       ) do
    push_event(socket, "section_deleted", %{section_id: section_id})
  end

  defp after_operation(socket, _prev_socket, {:insert_cell, client_id, _, _, _, cell_id, _attrs}) do
    {:ok, cell, _section} = Notebook.fetch_cell_and_section(socket.private.data.notebook, cell_id)

    socket = push_cell_editor_payloads(socket, socket.private.data, [cell])

    socket = prune_cell_sources(socket)

    if client_id == socket.assigns.client_id do
      push_event(socket, "cell_inserted", %{cell_id: cell_id})
    else
      socket
    end
  end

  defp after_operation(socket, prev_socket, {:delete_cell, _client_id, cell_id}) do
    # Find a sibling cell that the client would focus if the deleted cell has focus.
    sibling_cell_id =
      case Notebook.fetch_cell_sibling(prev_socket.private.data.notebook, cell_id, 1) do
        {:ok, next_cell} ->
          next_cell.id

        :error ->
          case Notebook.fetch_cell_sibling(prev_socket.private.data.notebook, cell_id, -1) do
            {:ok, previous_cell} -> previous_cell.id
            :error -> nil
          end
      end

    push_event(socket, "cell_deleted", %{cell_id: cell_id, sibling_cell_id: sibling_cell_id})
  end

  defp after_operation(socket, _prev_socket, {:restore_cell, client_id, cell_id}) do
    {:ok, cell, _section} = Notebook.fetch_cell_and_section(socket.private.data.notebook, cell_id)

    socket = push_cell_editor_payloads(socket, socket.private.data, [cell])

    socket = prune_cell_sources(socket)

    if client_id == socket.assigns.client_id do
      push_event(socket, "cell_restored", %{cell_id: cell_id})
    else
      socket
    end
  end

  defp after_operation(socket, _prev_socket, {:move_cell, client_id, cell_id, _offset}) do
    if client_id == socket.assigns.client_id do
      push_event(socket, "cell_moved", %{cell_id: cell_id})
    else
      socket
    end
  end

  defp after_operation(socket, _prev_socket, {:move_section, client_id, section_id, _offset}) do
    if client_id == socket.assigns.client_id do
      push_event(socket, "section_moved", %{section_id: section_id})
    else
      socket
    end
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:add_cell_evaluation_output, _client_id, _cell_id, _output}
       ) do
    prune_outputs(socket)
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:add_cell_evaluation_response, _client_id, cell_id, _output, metadata}
       ) do
    socket
    |> prune_outputs()
    |> push_event("evaluation_finished:#{cell_id}", %{code_markers: metadata.code_markers})
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:add_cell_doctest_report, _client_id, cell_id, doctest_report}
       ) do
    push_event(socket, "doctest_report:#{cell_id}", doctest_report_payload(doctest_report))
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:smart_cell_started, _client_id, cell_id, _delta, _chunks, _js_view, _editor}
       ) do
    {:ok, cell, _section} = Notebook.fetch_cell_and_section(socket.private.data.notebook, cell_id)

    socket
    |> push_cell_editor_payloads(socket.private.data, [cell], [:secondary])
    |> prune_cell_sources()
  end

  defp after_operation(socket, _prev_socket, {:erase_outputs, _client_id}) do
    push_event(socket, "erase_outputs", %{})
  end

  defp after_operation(socket, prev_socket, {:set_deployed_app_slug, _client_id, slug}) do
    prev_slug = prev_socket.private.data.deployed_app_slug

    if slug == prev_slug do
      socket
    else
      if prev_slug do
        Livebook.App.unsubscribe(prev_slug)
      end

      app =
        if slug do
          {:ok, app} = Livebook.Apps.fetch_app(slug)
          Livebook.App.subscribe(slug)
          app
        end

      assign(socket, app: app)
    end
  end

  defp after_operation(socket, _prev_socket, _operation), do: socket

  defp handle_actions(socket, actions) do
    Enum.reduce(actions, socket, &handle_action(&2, &1))
  end

  defp handle_action(socket, {:report_delta, client_id, cell, tag, delta}) do
    if client_id == socket.assigns.client_id do
      push_event(socket, "cell_acknowledgement:#{cell.id}:#{tag}", %{})
    else
      push_event(socket, "cell_delta:#{cell.id}:#{tag}", %{delta: Delta.to_compressed(delta)})
    end
  end

  defp handle_action(socket, {:start_evaluation, cell, _section}) do
    push_event(socket, "start_evaluation:#{cell.id}", %{})
  end

  defp handle_action(socket, _action), do: socket

  defp client_info(id, user) do
    %{id: id, hex_color: user.hex_color, name: user.name || "Anonymous"}
  end

  defp normalize_name(name) do
    name
    |> String.trim()
    |> String.replace(~r/\s+/, " ")
    |> case do
      "" -> "Untitled"
      name -> name
    end
  end

  def upcase_first(string) do
    {head, tail} = String.split_at(string, 1)
    String.upcase(head) <> tail
  end

  defp insert_cell_below(socket, params) do
    {type, attrs} = cell_type_and_attrs_from_params(params, socket)

    with {:ok, section, index} <-
           section_with_next_index(
             socket.private.data.notebook,
             params["section_id"],
             params["cell_id"]
           ) do
      Session.insert_cell(socket.assigns.session.pid, section.id, index, type, attrs)
    end

    socket
  end

  defp cell_type_and_attrs_from_params(%{"type" => "markdown"}, _socket), do: {:markdown, %{}}

  defp cell_type_and_attrs_from_params(%{"type" => "code"} = params, socket) do
    language =
      case params["language"] do
        language when language in ["elixir", "erlang"] ->
          String.to_atom(language)

        _ ->
          socket.private.data.notebook.default_language
      end

    {:code, %{language: language}}
  end

  defp cell_type_and_attrs_from_params(%{"type" => "diagram"}, _socket) do
    source = """
    <!-- Learn more at https://mermaid-js.github.io/mermaid -->

    ```mermaid
    graph TD;
      A-->B;
      A-->C;
      B-->D;
      C-->D;
    ```\
    """

    {:markdown, %{source: source}}
  end

  defp cell_type_and_attrs_from_params(%{"type" => "image", "url" => url}, _socket) do
    source = "![](#{url})"

    {:markdown, %{source: source}}
  end

  defp section_with_next_index(notebook, section_id, cell_id)

  defp section_with_next_index(notebook, section_id, nil) do
    with {:ok, section} <- Notebook.fetch_section(notebook, section_id) do
      {:ok, section, 0}
    end
  end

  defp section_with_next_index(notebook, _section_id, cell_id) do
    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(notebook, cell_id) do
      index = Enum.find_index(section.cells, &(&1 == cell))
      {:ok, section, index + 1}
    end
  end

  defp ensure_integer(n) when is_integer(n), do: n
  defp ensure_integer(n) when is_binary(n), do: String.to_integer(n)

  defp encode_digest(nil), do: nil
  defp encode_digest(digest), do: Base.encode64(digest)

  defp process_intellisense_response(
         %{range: %{from: from, to: to}} = response,
         {:details, line, _column}
       ) do
    %{
      response
      | range: %{
          from: JSInterop.elixir_column_to_js(from, line),
          to: JSInterop.elixir_column_to_js(to, line)
        }
    }
  end

  # Currently we don't use signature docs, so we optimise the response
  # to exclude them
  defp process_intellisense_response(
         %{signature_items: signature_items} = response,
         {:signature, _hint}
       ) do
    %{response | signature_items: Enum.map(signature_items, &%{&1 | documentation: nil})}
  end

  defp process_intellisense_response(response, _request), do: response

  defp autofocus_cell_id(%Notebook{sections: [%{cells: [%{id: id, source: ""}]}]}), do: id
  defp autofocus_cell_id(_notebook), do: nil

  defp connect_runtime(socket) do
    case Runtime.connect(socket.private.data.runtime) do
      {:ok, runtime} ->
        Session.set_runtime(socket.assigns.session.pid, runtime)
        {:ok, socket}

      {:error, message} ->
        {:error, put_flash(socket, :error, "Failed to connect runtime - #{message}")}
    end
  end

  defp maybe_reconnect_runtime(%{private: %{data: data}} = socket) do
    if Runtime.connected?(data.runtime) do
      data.runtime
      |> Runtime.duplicate()
      |> Runtime.connect()
      |> case do
        {:ok, new_runtime} ->
          Session.set_runtime(socket.assigns.session.pid, new_runtime)
          {:ok, clear_flash(socket, :error)}

        {:error, message} ->
          {:error, put_flash(socket, :error, "Failed to connect runtime - #{message}")}
      end
    else
      {:ok, socket}
    end
  end

  defp confirm_setup_default_runtime(socket, reason) do
    on_confirm = fn socket ->
      {status, socket} = connect_runtime(socket)

      if status == :ok do
        Session.queue_cell_evaluation(socket.assigns.session.pid, Cell.setup_cell_id())
      end

      socket
    end

    confirm(socket, on_confirm,
      title: "Setup runtime",
      description: "#{reason} Do you want to connect and setup the default one?",
      confirm_text: "Setup runtime",
      confirm_icon: "play-line",
      danger: false
    )
  end

  defp starred_files(starred_notebooks) do
    for info <- starred_notebooks, into: MapSet.new(), do: info.file
  end

  defp example_snippet_definition_by_name(data, name) do
    data.runtime
    |> Livebook.Runtime.snippet_definitions()
    |> Enum.find_value(:error, &(&1.type == :example && &1.name == name && {:ok, &1}))
  end

  defp smart_cell_definition_by_kind(data, kind) do
    Enum.find_value(data.smart_cell_definitions, :error, &(&1.kind == kind && {:ok, &1}))
  end

  defp add_dependencies_and_reevaluate(socket, dependencies) do
    Session.add_dependencies(socket.assigns.session.pid, dependencies)

    {status, socket} = maybe_reconnect_runtime(socket)

    if status == :ok do
      Session.queue_cell_evaluation(socket.assigns.session.pid, Cell.setup_cell_id())
      Session.queue_cells_reevaluation(socket.assigns.session.pid)
    end

    socket
  end

  defp ensure_packages_then(socket, packages, target_name, target_type, fun) do
    dependencies = Enum.map(packages, & &1.dependency)

    has_dependencies? =
      dependencies == [] or
        Livebook.Runtime.has_dependencies?(socket.private.data.runtime, dependencies)

    cond do
      has_dependencies? ->
        case fun.(socket) do
          {:ok, socket} -> socket
          :error -> socket
        end

      Livebook.Runtime.fixed_dependencies?(socket.private.data.runtime) ->
        put_flash(socket, :error, "This runtime doesn't support adding dependencies")

      true ->
        on_confirm = fn socket ->
          case fun.(socket) do
            {:ok, socket} -> add_dependencies_and_reevaluate(socket, dependencies)
            :error -> socket
          end
        end

        confirm_add_packages(socket, on_confirm, packages, target_name, target_type)
    end
  end

  defp confirm_add_packages(socket, on_confirm, packages, target_name, target_type) do
    assigns = %{packages: packages, target_name: target_name, target_type: target_type}

    description = ~H"""
    The <span class="font-semibold">“<%= @target_name %>“</span> <%= @target_type %> requires the
    <.listing items={@packages}>
      <:item :let={package}><code><%= package.name %></code></:item>
      <:singular_suffix>package. Do you want to add it as a dependency and restart?</:singular_suffix>
      <:plural_suffix>packages. Do you want to add them as dependencies and restart?</:plural_suffix>
    </.listing>
    """

    confirm(socket, on_confirm,
      title: "Add packages",
      description: description,
      confirm_text: "Add and restart",
      confirm_icon: "add-line",
      danger: false
    )
  end

  defp push_cell_editor_payloads(socket, data, cells, tags \\ :all) do
    for cell <- cells,
        {tag, payload} <- cell_editor_init_payloads(cell, data.cell_infos[cell.id]),
        tags == :all or tag in tags,
        reduce: socket do
      socket ->
        push_event(socket, "cell_editor_init:#{cell.id}:#{tag}", payload)
    end
  end

  defp cell_editor_init_payloads(%Cell.Code{} = cell, cell_info) do
    [
      primary: %{
        source: cell.source,
        revision: cell_info.sources.primary.revision,
        code_markers: cell_info.eval.code_markers,
        doctest_reports:
          for {_, doctest_report} <- cell_info.eval.doctest_reports do
            doctest_report_payload(doctest_report)
          end
      }
    ]
  end

  defp cell_editor_init_payloads(%Cell.Markdown{} = cell, cell_info) do
    [
      primary: %{
        source: cell.source,
        revision: cell_info.sources.primary.revision,
        code_markers: [],
        doctest_reports: []
      }
    ]
  end

  defp cell_editor_init_payloads(%Cell.Smart{} = cell, cell_info) do
    [
      primary: %{
        source: cell.source,
        revision: cell_info.sources.primary.revision,
        code_markers: cell_info.eval.code_markers,
        doctest_reports:
          for {_, doctest_report} <- cell_info.eval.doctest_reports do
            doctest_report_payload(doctest_report)
          end
      }
    ] ++
      if cell.editor && cell_info.status == :started do
        [
          secondary: %{
            source: cell.editor.source,
            revision: cell_info.sources.secondary.revision,
            code_markers: [],
            doctest_reports: []
          }
        ]
      else
        []
      end
  end

  defp doctest_report_payload(doctest_report) do
    Map.replace_lazy(doctest_report, :details, fn details ->
      details
      |> LivebookWeb.Helpers.ANSI.ansi_string_to_html_lines()
      |> Enum.map(&Phoenix.HTML.safe_to_string/1)
    end)
  end

  defp find_file_entry(socket, name) do
    Enum.find(socket.private.data.notebook.file_entries, &(&1.name == name))
  end

  defp handlers_for_file_entry(file_entry, runtime) do
    handlers =
      for definition <- Livebook.Runtime.snippet_definitions(runtime),
          definition.type == :file_action,
          do: %{definition: definition, cell_type: :code}

    handlers =
      if file_entry.type == :attachment do
        [
          %{
            cell_type: :markdown,
            definition: %{
              type: :file_action,
              description: "Insert as Markdown image",
              source: "![](files/{{NAME}})",
              file_types: ["image/*"],
              packages: []
            }
          }
          | handlers
        ]
      else
        handlers
      end

    handlers
    |> Enum.filter(&matches_file_types?(file_entry.name, &1.definition.file_types))
    |> Enum.sort_by(& &1.definition.description)
  end

  defp matches_file_types?(_name, :any), do: true

  defp matches_file_types?(name, file_types) do
    mime_type = MIME.from_path(name)
    extension = Path.extname(name)

    Enum.any?(file_types, fn file_type ->
      case String.split(file_type, "/") do
        [group, "*"] ->
          String.starts_with?(mime_type, group <> "/")

        _ ->
          file_type == mime_type or file_type == extension
      end
    end)
  end

  # Builds view-specific structure of data by cherry-picking
  # only the relevant attributes.
  # We then use `@data_view` in the templates and consequently
  # irrelevant changes to data don't change `@data_view`, so LV doesn't
  # have to traverse the whole template tree and no diff is sent to the client.
  defp data_to_view(data) do
    changed_input_ids = Session.Data.changed_input_ids(data)

    %{
      file: data.file,
      persist_outputs: data.notebook.persist_outputs,
      autosave_interval_s: data.notebook.autosave_interval_s,
      default_language: data.notebook.default_language,
      dirty: data.dirty,
      persistence_warnings: data.persistence_warnings,
      runtime: data.runtime,
      smart_cell_definitions: Enum.sort_by(data.smart_cell_definitions, & &1.name),
      example_snippet_definitions:
        data.runtime
        |> Livebook.Runtime.snippet_definitions()
        |> Enum.filter(&(&1.type == :example))
        |> Enum.sort_by(& &1.name),
      global_status: global_status(data),
      notebook_name: data.notebook.name,
      sections_items:
        for section <- data.notebook.sections do
          %{
            id: section.id,
            name: section.name,
            parent: parent_section_view(section.parent_id, data),
            status: cells_status(section.cells, data)
          }
        end,
      clients:
        data.clients_map
        |> Enum.map(fn {client_id, user_id} -> {client_id, data.users_map[user_id]} end)
        |> Enum.sort_by(fn {_client_id, user} -> user.name end),
      installing?: data.cell_infos[Cell.setup_cell_id()].eval.status == :evaluating,
      setup_cell_view: %{
        cell_to_view(hd(data.notebook.setup_section.cells), data, changed_input_ids)
        | type: :setup
      },
      section_views: section_views(data.notebook.sections, data, changed_input_ids),
      bin_entries: data.bin_entries,
      secrets: data.secrets,
      hub: Livebook.Hubs.fetch_hub!(data.notebook.hub_id),
      hub_secrets: data.hub_secrets,
      any_session_secrets?:
        Session.Data.session_secrets(data.secrets, data.notebook.hub_id) != [],
      file_entries: Enum.sort_by(data.notebook.file_entries, & &1.name),
      quarantine_file_entry_names: data.notebook.quarantine_file_entry_names,
      app_settings: data.notebook.app_settings,
      deployed_app_slug: data.deployed_app_slug
    }
  end

  defp cells_status(cells, data) do
    eval_infos =
      for cell <- cells,
          Cell.evaluable?(cell),
          info = data.cell_infos[cell.id].eval,
          do: Map.put(info, :id, cell.id)

    most_recent =
      eval_infos
      |> Enum.filter(& &1.evaluation_end)
      |> Enum.max_by(& &1.evaluation_end, DateTime, fn -> nil end)

    cond do
      evaluating = Enum.find(eval_infos, &(&1.status == :evaluating)) ->
        {:evaluating, evaluating.id}

      most_recent != nil and most_recent.errored ->
        {:errored, most_recent.id}

      stale = Enum.find(eval_infos, &(&1.validity == :stale)) ->
        {:stale, stale.id}

      most_recent != nil ->
        {:evaluated, most_recent.id}

      true ->
        {:fresh, nil}
    end
  end

  defp global_status(data) do
    cells =
      data.notebook
      |> Notebook.evaluable_cells_with_section()
      |> Enum.map(fn {cell, _} -> cell end)

    cells_status(cells, data)
  end

  defp section_views(sections, data, changed_input_ids) do
    sections
    |> Enum.map(& &1.name)
    |> names_to_html_ids()
    |> Enum.zip(sections)
    |> Enum.map(fn {html_id, section} ->
      %{
        id: section.id,
        html_id: html_id,
        name: section.name,
        parent: parent_section_view(section.parent_id, data),
        has_children?: Notebook.child_sections(data.notebook, section.id) != [],
        valid_parents:
          for parent <- Notebook.valid_parents_for(data.notebook, section.id) do
            %{id: parent.id, name: parent.name}
          end,
        cell_views: Enum.map(section.cells, &cell_to_view(&1, data, changed_input_ids))
      }
    end)
  end

  defp parent_section_view(nil, _data), do: nil

  defp parent_section_view(parent_id, data) do
    {:ok, section} = Notebook.fetch_section(data.notebook, parent_id)
    %{id: section.id, name: section.name}
  end

  defp cell_to_view(%Cell.Markdown{} = cell, _data, _changed_input_ids) do
    %{
      id: cell.id,
      type: :markdown,
      empty: cell.source == ""
    }
  end

  defp cell_to_view(%Cell.Code{} = cell, data, changed_input_ids) do
    info = data.cell_infos[cell.id]

    %{
      id: cell.id,
      type: :code,
      language: cell.language,
      empty: cell.source == "",
      eval: eval_info_to_view(cell, info.eval, data, changed_input_ids),
      reevaluate_automatically: cell.reevaluate_automatically
    }
  end

  defp cell_to_view(%Cell.Smart{} = cell, data, changed_input_ids) do
    info = data.cell_infos[cell.id]

    %{
      id: cell.id,
      type: :smart,
      empty: cell.source == "",
      eval: eval_info_to_view(cell, info.eval, data, changed_input_ids),
      status: info.status,
      js_view: cell.js_view,
      editor:
        cell.editor &&
          %{
            empty: cell.editor.source == "",
            language: cell.editor.language,
            placement: cell.editor.placement
          }
    }
  end

  defp eval_info_to_view(cell, eval_info, data, changed_input_ids) do
    %{
      outputs: cell.outputs,
      doctest_summary: doctest_summary(eval_info.doctest_reports),
      validity: eval_info.validity,
      status: eval_info.status,
      errored: eval_info.errored,
      reevaluates_automatically: eval_info.reevaluates_automatically,
      evaluation_time_ms: eval_info.evaluation_time_ms,
      evaluation_start: eval_info.evaluation_start,
      evaluation_digest: encode_digest(eval_info.evaluation_digest),
      outputs_batch_number: eval_info.outputs_batch_number,
      # Pass input values relevant to the given cell
      input_views: input_views_for_cell(cell, data, changed_input_ids)
    }
  end

  defp doctest_summary(doctests) do
    {doctests_count, failures_count} =
      Enum.reduce(doctests, {0, 0}, fn
        {_line, %{status: status}}, {total, failed} ->
          {total + 1, if(status == :failed, do: failed + 1, else: failed)}
      end)

    %{doctests_count: doctests_count, failures_count: failures_count}
  end

  defp input_views_for_cell(cell, data, changed_input_ids) do
    input_ids =
      for output <- cell.outputs,
          attrs <- Cell.find_inputs_in_output(output),
          do: attrs.id

    data.input_infos
    |> Map.take(input_ids)
    |> Map.new(fn {input_id, %{value: value}} ->
      {input_id, %{value: value, changed: MapSet.member?(changed_input_ids, input_id)}}
    end)
  end

  def app_status(%{sessions: [app_session | _]}), do: app_session.app_status
  def app_status(_), do: nil

  # Updates current data_view in response to an operation.
  # In most cases we simply recompute data_view, but for the
  # most common ones we only update the relevant parts.
  defp update_data_view(data_view, prev_data, data, operation) do
    case operation do
      {:report_cell_revision, _client_id, _cell_id, _tag, _revision} ->
        data_view

      {:apply_cell_delta, _client_id, _cell_id, _tag, _delta, _revision} ->
        update_dirty_status(data_view, data)

      {:update_smart_cell, _client_id, _cell_id, _cell_state, _delta, _chunks} ->
        update_dirty_status(data_view, data)

      # For outputs that update existing outputs we send the update directly
      # to the corresponding component, so the DOM patch is isolated and fast.
      # This is important for intensive output updates
      {:add_cell_evaluation_output, _client_id, _cell_id,
       {:frame, _outputs, %{type: type, ref: ref}}}
      when type != :default ->
        for {idx, {:frame, frame_outputs, _}} <- Notebook.find_frame_outputs(data.notebook, ref) do
          send_update(LivebookWeb.Output.FrameComponent,
            id: "output-#{idx}",
            outputs: frame_outputs,
            update_type: type
          )
        end

        data_view

      {:add_cell_evaluation_output, _client_id, cell_id, {:stdout, text}} ->
        # Lookup in previous data to see if the output is already there
        case Notebook.fetch_cell_and_section(prev_data.notebook, cell_id) do
          {:ok, %{outputs: [{idx, {:stdout, _}} | _]}, _section} ->
            send_update(LivebookWeb.Output.StdoutComponent, id: "output-#{idx}", text: text)
            data_view

          _ ->
            data_to_view(data)
        end

      {:add_cell_doctest_report, _client_id, _cell_id, _doctest_report} ->
        data_view

      _ ->
        data_to_view(data)
    end
  end

  defp prune_outputs(%{private: %{data: data}} = socket) do
    assign_private(
      socket,
      data: update_in(data.notebook, &Notebook.prune_cell_outputs/1)
    )
  end

  defp prune_cell_sources(%{private: %{data: data}} = socket) do
    assign_private(
      socket,
      data:
        update_in(
          data.notebook,
          &Notebook.update_cells(&1, fn
            %Notebook.Cell.Smart{} = cell ->
              %{cell | source: :__pruned__, attrs: :__pruned__}
              |> prune_smart_cell_editor_source()

            %{source: _} = cell ->
              %{cell | source: :__pruned__}

            cell ->
              cell
          end)
        )
    )
  end

  defp prune_smart_cell_editor_source(%{editor: %{source: _}} = cell),
    do: put_in(cell.editor.source, :__pruned__)

  defp prune_smart_cell_editor_source(cell), do: cell

  # Changes that affect only a single cell are still likely to
  # have impact on dirtiness, so we need to always mirror it
  defp update_dirty_status(data_view, data) do
    put_in(data_view.dirty, data.dirty)
  end

  defp get_page_title(notebook_name) do
    "#{notebook_name} - Livebook"
  end

  defp memory_color(:atom), do: "bg-blue-500"
  defp memory_color(:code), do: "bg-yellow-600"
  defp memory_color(:processes), do: "bg-blue-700"
  defp memory_color(:binary), do: "bg-green-500"
  defp memory_color(:ets), do: "bg-red-500"
  defp memory_color(:other), do: "bg-gray-400"

  defp runtime_memory(%{runtime: memory}) do
    memory
    |> Map.drop([:total, :system])
    |> Enum.map(fn {type, bytes} ->
      {type,
       %{
         unit: format_bytes(bytes),
         percentage: Float.round(bytes / memory.total * 100, 2),
         value: bytes
       }}
    end)
  end

  defp app_status_color(nil), do: "bg-gray-400"
  defp app_status_color(%{lifecycle: :shutting_down}), do: "bg-gray-500"
  defp app_status_color(%{lifecycle: :deactivated}), do: "bg-gray-500"
  defp app_status_color(%{execution: :executing}), do: "bg-blue-500"
  defp app_status_color(%{execution: :executed}), do: "bg-green-bright-400"
  defp app_status_color(%{execution: :error}), do: "bg-red-400"
  defp app_status_color(%{execution: :interrupted}), do: "bg-gray-400"
end

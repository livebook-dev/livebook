defmodule LivebookWeb.SessionLive do
  use LivebookWeb, :live_view

  import LivebookWeb.UserHelpers
  import LivebookWeb.SessionHelpers
  import Livebook.Utils, only: [format_bytes: 1]

  alias LivebookWeb.SidebarHelpers
  alias Livebook.{Sessions, Session, Delta, Notebook, Runtime, LiveMarkdown}
  alias Livebook.Notebook.Cell
  alias Livebook.JSInterop

  @impl true
  def mount(%{"id" => session_id}, _session, socket) do
    # We use the tracked sessions to locate the session pid, but then
    # we talk to the session process exclusively for getting all the information
    case Sessions.fetch_session(session_id) do
      {:ok, %{pid: session_pid}} ->
        data =
          if connected?(socket) do
            data = Session.register_client(session_pid, self(), socket.assigns.current_user)
            Session.subscribe(session_id)

            data
          else
            Session.get_data(session_pid)
          end

        socket =
          if connected?(socket) do
            payload = %{
              clients:
                Enum.map(data.clients_map, fn {client_pid, user_id} ->
                  client_info(client_pid, data.users_map[user_id])
                end)
            }

            push_event(socket, "session_init", payload)
          else
            socket
          end

        session = Session.get_by_pid(session_pid)

        platform = platform_from_socket(socket)

        {:ok,
         socket
         |> assign(
           self_path: Routes.session_path(socket, :page, session.id),
           session: session,
           platform: platform,
           self: self(),
           data_view: data_to_view(data),
           autofocus_cell_id: autofocus_cell_id(data.notebook),
           page_title: get_page_title(data.notebook.name)
         )
         |> assign_private(data: data)
         |> prune_outputs()
         |> prune_cell_sources()
         |> allow_upload(:cell_image,
           accept: ~w(.jpg .jpeg .png .gif),
           max_entries: 1,
           max_file_size: 5_000_000
         )}

      :error ->
        {:ok, redirect(socket, to: Routes.home_path(socket, :page))}
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
      <SidebarHelpers.sidebar>
        <SidebarHelpers.logo_item socket={@socket} />
        <SidebarHelpers.button_item
          icon="booklet-fill"
          label="Sections (ss)"
          button_attrs={[data_el_sections_list_toggle: true]}
        />
        <SidebarHelpers.button_item
          icon="group-fill"
          label="Connected users (su)"
          button_attrs={[data_el_clients_list_toggle: true]}
        />
        <SidebarHelpers.button_item
          icon="cpu-line"
          label="Runtime settings (sr)"
          button_attrs={[data_el_runtime_info_toggle: true]}
        />
        <SidebarHelpers.link_item
          icon="delete-bin-6-fill"
          label="Bin (sb)"
          path={Routes.session_path(@socket, :bin, @session.id)}
          active={@live_action == :bin}
          link_attrs={[data_btn_show_bin: true]}
        />
        <SidebarHelpers.break_item />
        <SidebarHelpers.link_item
          icon="keyboard-box-fill"
          label="Keyboard shortcuts (?)"
          path={Routes.session_path(@socket, :shortcuts, @session.id)}
          active={@live_action == :shortcuts}
          link_attrs={[data_btn_show_shortcuts: true]}
        />
        <SidebarHelpers.user_item current_user={@current_user} />
      </SidebarHelpers.sidebar>
      <div
        class="flex flex-col h-full w-full max-w-xs absolute z-30 top-0 left-[64px] overflow-y-auto shadow-xl md:static md:shadow-none bg-gray-50 border-r border-gray-100 px-6 py-10"
        data-el-side-panel
      >
        <div data-el-sections-list>
          <.sections_list data_view={@data_view} />
        </div>
        <div data-el-clients-list>
          <.clients_list data_view={@data_view} self={@self} />
        </div>
        <div data-el-runtime-info>
          <.runtime_info data_view={@data_view} session={@session} socket={@socket} />
        </div>
      </div>
      <div class="grow overflow-y-auto relative" data-el-notebook>
        <div data-el-js-view-iframes phx-update="ignore" id="js-view-iframes"></div>
        <LivebookWeb.SessionLive.IndicatorsComponent.render
          socket={@socket}
          session_id={@session.id}
          file={@data_view.file}
          dirty={@data_view.dirty}
          autosave_interval_s={@data_view.autosave_interval_s}
          runtime={@data_view.runtime}
          global_status={@data_view.global_status}
        />
        <div
          class="w-full max-w-screen-lg px-4 sm:pl-8 sm:pr-16 md:pl-16 pt-4 sm:py-7 mx-auto"
          data-el-notebook-content
        >
          <div
            class="flex items-center pb-4 mb-2 space-x-4 border-b border-gray-200"
            data-el-notebook-headline
            data-focusable-id="notebook"
            id="notebook"
            phx-hook="Headline"
            data-on-value-change="set_notebook_name"
            data-metadata="notebook"
          >
            <h1
              class="grow p-1 -ml-1 text-3xl font-semibold text-gray-800 border border-transparent rounded-lg whitespace-pre-wrap"
              tabindex="0"
              id="notebook-heading"
              data-el-heading
              spellcheck="false"
              phx-no-format
            ><%= @data_view.notebook_name %></h1>
            <.menu id="session-menu">
              <:toggle>
                <button class="icon-button" aria-label="open notebook menu">
                  <.remix_icon icon="more-2-fill" class="text-xl" />
                </button>
              </:toggle>
              <:content>
                <%= live_patch to: Routes.session_path(@socket, :export, @session.id, "livemd"),
                      class: "menu-item text-gray-500",
                      role: "menuitem" do %>
                  <.remix_icon icon="download-2-line" />
                  <span class="font-medium">Export</span>
                <% end %>
                <button class="menu-item text-gray-500" role="menuitem" phx-click="erase_outputs">
                  <.remix_icon icon="eraser-fill" />
                  <span class="font-medium">Erase outputs</span>
                </button>
                <button class="menu-item text-gray-500" role="menuitem" phx-click="fork_session">
                  <.remix_icon icon="git-branch-line" />
                  <span class="font-medium">Fork</span>
                </button>
                <a
                  class="menu-item text-gray-500"
                  role="menuitem"
                  href={live_dashboard_process_path(@socket, @session.pid)}
                  target="_blank"
                >
                  <.remix_icon icon="dashboard-2-line" />
                  <span class="font-medium">See on Dashboard</span>
                </a>
                <%= live_redirect to: Routes.home_path(@socket, :close_session, @session.id),
                      class: "menu-item text-red-600",
                      role: "menuitem" do %>
                  <.remix_icon icon="close-circle-line" />
                  <span class="font-medium">Close</span>
                <% end %>
              </:content>
            </.menu>
          </div>
          <div>
            <.live_component
              module={LivebookWeb.SessionLive.CellComponent}
              id={@data_view.setup_cell_view.id}
              session_id={@session.id}
              runtime={@data_view.runtime}
              installing?={@data_view.installing?}
              cell_view={@data_view.setup_cell_view}
            />
          </div>
          <div class="mt-8 flex flex-col w-full space-y-16" data-el-sections-container>
            <%= if @data_view.section_views == [] do %>
              <div class="flex justify-center">
                <button class="button-base button-small" phx-click="append_section">
                  + Section
                </button>
              </div>
            <% end %>
            <%= for {section_view, index} <- Enum.with_index(@data_view.section_views) do %>
              <.live_component
                module={LivebookWeb.SessionLive.SectionComponent}
                id={section_view.id}
                index={index}
                session_id={@session.id}
                runtime={@data_view.runtime}
                smart_cell_definitions={@data_view.smart_cell_definitions}
                installing?={@data_view.installing?}
                section_view={section_view}
              />
            <% end %>
            <div style="height: 80vh"></div>
          </div>
        </div>
      </div>
    </div>

    <.current_user_modal return_to={@self_path} current_user={@current_user} />

    <%= if @live_action == :runtime_settings do %>
      <.modal id="runtime-settings-modal" show class="w-full max-w-4xl" patch={@self_path}>
        <.live_component
          module={LivebookWeb.SessionLive.RuntimeComponent}
          id="runtime-settings"
          session={@session}
          runtime={@data_view.runtime}
        />
      </.modal>
    <% end %>

    <%= if @live_action == :file_settings do %>
      <.modal id="persistence-modal" show class="w-full max-w-4xl" patch={@self_path}>
        <%= live_render(@socket, LivebookWeb.SessionLive.PersistenceLive,
          id: "persistence",
          session: %{
            "session" => @session,
            "file" => @data_view.file,
            "persist_outputs" => @data_view.persist_outputs,
            "autosave_interval_s" => @data_view.autosave_interval_s
          }
        ) %>
      </.modal>
    <% end %>

    <%= if @live_action == :shortcuts do %>
      <.modal id="shortcuts-modal" show class="w-full max-w-6xl" patch={@self_path}>
        <.live_component
          module={LivebookWeb.SessionLive.ShortcutsComponent}
          id="shortcuts"
          platform={@platform}
        />
      </.modal>
    <% end %>

    <%= if @live_action == :cell_settings do %>
      <.modal id="cell-settings-modal" show class="w-full max-w-xl" patch={@self_path}>
        <.live_component
          module={settings_component_for(@cell)}
          id="cell-settings"
          session={@session}
          return_to={@self_path}
          cell={@cell}
        />
      </.modal>
    <% end %>

    <%= if @live_action == :cell_upload do %>
      <.modal id="cell-upload-modal" show class="w-full max-w-xl" patch={@self_path}>
        <.live_component
          module={LivebookWeb.SessionLive.CellUploadComponent}
          id="cell-upload"
          session={@session}
          return_to={@self_path}
          cell={@cell}
          uploads={@uploads}
        />
      </.modal>
    <% end %>

    <%= if @live_action == :delete_section do %>
      <.modal id="delete-section-modal" show class="w-full max-w-xl" patch={@self_path}>
        <.live_component
          module={LivebookWeb.SessionLive.DeleteSectionComponent}
          id="delete-section"
          session={@session}
          return_to={@self_path}
          section={@section}
          is_first={@section.id == @first_section_id}
        />
      </.modal>
    <% end %>

    <%= if @live_action == :bin do %>
      <.modal id="bin-modal" show class="w-full max-w-4xl" patch={@self_path}>
        <.live_component
          module={LivebookWeb.SessionLive.BinComponent}
          id="bin"
          session={@session}
          return_to={@self_path}
          bin_entries={@data_view.bin_entries}
        />
      </.modal>
    <% end %>

    <%= if @live_action == :export do %>
      <.modal id="export-modal" show class="w-full max-w-4xl" patch={@self_path}>
        <.live_component
          module={LivebookWeb.SessionLive.ExportComponent}
          id="export"
          session={@session}
          tab={@tab}
        />
      </.modal>
    <% end %>

    <%= if @live_action == :package_search do %>
      <.modal id="package-search-modal" show class="w-full max-w-xl" patch={@self_path}>
        <%= live_render(@socket, LivebookWeb.SessionLive.PackageSearchLive,
          id: "package-search",
          session: %{
            "session" => @session,
            "runtime" => @data_view.runtime,
            "return_to" => @self_path
          }
        ) %>
      </.modal>
    <% end %>
    """
  end

  defp sections_list(assigns) do
    ~H"""
    <div class="flex flex-col grow">
      <h3 class="uppercase text-sm font-semibold text-gray-500">
        Sections
      </h3>
      <div class="flex flex-col mt-4 space-y-4">
        <%= for section_item <- @data_view.sections_items do %>
          <div class="flex items-center">
            <button
              class="grow flex items-center text-gray-500 hover:text-gray-900 text-left"
              data-el-sections-list-item
              data-section-id={section_item.id}
            >
              <span class="flex items-center space-x-1">
                <span><%= section_item.name %></span>
                <%= if section_item.parent do %>
                  <% # Note: the container has overflow-y auto, so we cannot set overflow-x visible,
                  # consequently we show the tooltip wrapped to a fixed number of characters %>
                  <span {branching_tooltip_attrs(section_item.name, section_item.parent.name)}>
                    <.remix_icon
                      icon="git-branch-line"
                      class="text-lg font-normal leading-none flip-horizontally"
                    />
                  </span>
                <% end %>
              </span>
            </button>
            <.session_status
              status={elem(section_item.status, 0)}
              cell_id={elem(section_item.status, 1)}
            />
          </div>
        <% end %>
      </div>
      <button
        class="inline-flex items-center justify-center p-8 py-1 mt-8 space-x-2 text-sm font-medium text-gray-500 border border-gray-400 border-dashed rounded-xl hover:bg-gray-100"
        phx-click="append_section"
      >
        <.remix_icon icon="add-line" class="text-lg align-center" />
        <span>New section</span>
      </button>
    </div>
    """
  end

  defp clients_list(assigns) do
    ~H"""
    <div class="flex flex-col grow">
      <div class="flex items-center justify-between space-x-4">
        <h3 class="uppercase text-sm font-semibold text-gray-500">
          Users
        </h3>
        <span class="flex items-center px-2 py-1 space-x-2 text-sm bg-gray-200 rounded-lg">
          <span class="inline-flex w-3 h-3 bg-green-600 rounded-full"></span>
          <span><%= length(@data_view.clients) %> connected</span>
        </span>
      </div>
      <div class="flex flex-col mt-4 space-y-4">
        <%= for {client_pid, user} <- @data_view.clients do %>
          <div
            class="flex items-center justify-between space-x-2"
            id={"clients-list-item-#{inspect(client_pid)}"}
            data-el-clients-list-item
            data-client-pid={inspect(client_pid)}
          >
            <button
              class="flex items-center space-x-2 text-gray-500 hover:text-gray-900 disabled:pointer-events-none"
              disabled={client_pid == @self}
              data-el-client-link
            >
              <.user_avatar user={user} class="shrink-0 h-7 w-7" text_class="text-xs" />
              <span><%= user.name || "Anonymous" %></span>
            </button>
            <%= if client_pid != @self do %>
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
        <% end %>
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
        <%= live_patch to: Routes.session_path(@socket, :runtime_settings, @session.id),
              class: "icon-button" do %>
          <.remix_icon icon="settings-3-line text-xl" />
        <% end %>
      </div>
      <div class="flex flex-col mt-2 space-y-4">
        <div class="flex flex-col space-y-3">
          <%= for {label, value} <- Runtime.describe(@data_view.runtime) do %>
            <.labeled_text label={label} one_line><%= value %></.labeled_text>
          <% end %>
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
            <%= live_patch to: Routes.session_path(@socket, :runtime_settings, @session.id),
                  class: "button-base button-outlined-gray bg-transparent" do %>
              Configure
            <% end %>
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
      <div class="mb-1 text-sm font-semibold text-gray-800 flex flex-row justify-between">
        <span class="text-gray-500 uppercase">Memory</span>
        <span class="text-right">
          <%= format_bytes(@memory_usage.system.free) %> available
        </span>
      </div>
      <div class="w-full h-8 flex flex-row py-1 gap-0.5">
        <%= for {type, memory} <- @runtime_memory  do %>
          <div class={"h-6 #{memory_color(type)}"} style={"width: #{memory.percentage}%"}></div>
        <% end %>
      </div>
      <div class="flex flex-col py-1">
        <%= for {type, memory} <- @runtime_memory do %>
          <div class="flex flex-row items-center">
            <span class={"w-4 h-4 mr-2 rounded #{memory_color(type)}"}></span>
            <span class="capitalize text-gray-700"><%= type %></span>
            <span class="text-gray-500 ml-auto"><%= memory.unit %></span>
          </div>
        <% end %>
        <div class="flex rounded justify-center my-2 py-0.5 text-sm text-gray-800 bg-gray-200">
          Total: <%= format_bytes(@memory_usage.runtime.total) %>
        </div>
      </div>
    </div>
    """
  end

  defp session_status(%{status: :evaluating} = assigns) do
    ~H"""
    <button data-el-focus-cell-button data-target={@cell_id}>
      <.status_indicator circle_class="bg-blue-500" animated_circle_class="bg-blue-400">
      </.status_indicator>
    </button>
    """
  end

  defp session_status(%{status: :stale} = assigns) do
    ~H"""
    <button data-el-focus-cell-button data-target={@cell_id}>
      <.status_indicator circle_class="bg-yellow-bright-200"></.status_indicator>
    </button>
    """
  end

  defp session_status(assigns), do: ~H"
"

  defp status_indicator(assigns) do
    assigns = assign_new(assigns, :animated_circle_class, fn -> nil end)

    ~H"""
    <div class="flex items-center space-x-1">
      <span class="flex relative h-3 w-3">
        <%= if @animated_circle_class do %>
          <span class={
            "#{@animated_circle_class} animate-ping absolute inline-flex h-3 w-3 rounded-full opacity-75"
          }>
          </span>
        <% end %>
        <span class={"#{@circle_class} relative inline-flex rounded-full h-3 w-3"}></span>
      </span>
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
      when socket.assigns.live_action in [:cell_settings, :cell_upload] do
    {:ok, cell, _} = Notebook.fetch_cell_and_section(socket.private.data.notebook, cell_id)
    {:noreply, assign(socket, cell: cell)}
  end

  def handle_params(%{"section_id" => section_id}, _url, socket)
      when socket.assigns.live_action == :delete_section do
    {:ok, section} = Notebook.fetch_section(socket.private.data.notebook, section_id)
    first_section_id = hd(socket.private.data.notebook.sections).id
    {:noreply, assign(socket, section: section, first_section_id: first_section_id)}
  end

  def handle_params(
        %{"path_parts" => path_parts},
        _url,
        %{assigns: %{live_action: :catch_all}} = socket
      ) do
    if socket.assigns.policy.edit do
      path_parts =
        Enum.map(path_parts, fn
          "__parent__" -> ".."
          part -> part
        end)

      path = Path.join(path_parts)
      {:noreply, handle_relative_path(socket, path)}
    else
      {:noreply, socket |> put_flash(:error, "No access to navigate") |> redirect_to_self()}
    end
  end

  def handle_params(%{"tab" => tab}, _url, socket)
      when socket.assigns.live_action == :export do
    {:noreply, assign(socket, tab: tab)}
  end

  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("append_section", %{}, socket) do
    assert_policy!(socket, :edit)
    idx = length(socket.private.data.notebook.sections)
    Session.insert_section(socket.assigns.session.pid, idx)

    {:noreply, socket}
  end

  def handle_event("insert_section_below", params, socket) do
    assert_policy!(socket, :edit)

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
    assert_policy!(socket, :edit)
    Session.set_section_parent(socket.assigns.session.pid, section_id, parent_id)

    {:noreply, socket}
  end

  def handle_event("unset_section_parent", %{"section_id" => section_id}, socket) do
    assert_policy!(socket, :edit)
    Session.unset_section_parent(socket.assigns.session.pid, section_id)

    {:noreply, socket}
  end

  def handle_event("insert_cell_below", params, socket) do
    assert_policy!(socket, :edit)
    {type, attrs} = cell_type_and_attrs_from_params(params)

    with {:ok, section, index} <-
           section_with_next_index(
             socket.private.data.notebook,
             params["section_id"],
             params["cell_id"]
           ) do
      Session.insert_cell(socket.assigns.session.pid, section.id, index, type, attrs)
    end

    {:noreply, socket}
  end

  def handle_event("delete_cell", %{"cell_id" => cell_id}, socket) do
    assert_policy!(socket, :edit)
    Session.delete_cell(socket.assigns.session.pid, cell_id)

    {:noreply, socket}
  end

  def handle_event("set_notebook_name", %{"value" => name}, socket) do
    assert_policy!(socket, :edit)
    name = normalize_name(name)
    Session.set_notebook_name(socket.assigns.session.pid, name)

    {:noreply, socket}
  end

  def handle_event("set_section_name", %{"metadata" => section_id, "value" => name}, socket) do
    assert_policy!(socket, :edit)
    name = normalize_name(name)
    Session.set_section_name(socket.assigns.session.pid, section_id, name)

    {:noreply, socket}
  end

  def handle_event(
        "apply_cell_delta",
        %{"cell_id" => cell_id, "tag" => tag, "delta" => delta, "revision" => revision},
        socket
      ) do
    assert_policy!(socket, :edit)
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
    assert_policy!(socket, :read)
    tag = String.to_atom(tag)
    Session.report_cell_revision(socket.assigns.session.pid, cell_id, tag, revision)

    {:noreply, socket}
  end

  def handle_event("move_cell", %{"cell_id" => cell_id, "offset" => offset}, socket) do
    assert_policy!(socket, :edit)
    offset = ensure_integer(offset)
    Session.move_cell(socket.assigns.session.pid, cell_id, offset)

    {:noreply, socket}
  end

  def handle_event("move_section", %{"section_id" => section_id, "offset" => offset}, socket) do
    assert_policy!(socket, :edit)
    offset = ensure_integer(offset)
    Session.move_section(socket.assigns.session.pid, section_id, offset)

    {:noreply, socket}
  end

  def handle_event("delete_section", %{"section_id" => section_id}, socket) do
    assert_policy!(socket, :edit)

    socket =
      case Notebook.fetch_section(socket.private.data.notebook, section_id) do
        {:ok, %{cells: []} = section} ->
          Session.delete_section(socket.assigns.session.pid, section.id, true)
          socket

        {:ok, section} ->
          push_patch(socket,
            to:
              Routes.session_path(
                socket,
                :delete_section,
                socket.assigns.session.id,
                section.id
              )
          )

        :error ->
          socket
      end

    {:noreply, socket}
  end

  def handle_event("convert_smart_cell", %{"cell_id" => cell_id}, socket) do
    assert_policy!(socket, :edit)
    Session.convert_smart_cell(socket.assigns.session.pid, cell_id)

    {:noreply, socket}
  end

  def handle_event(
        "add_smart_cell_dependencies",
        %{"kind" => kind, "variant_idx" => variant_idx},
        socket
      ) do
    assert_policy!(socket, :edit)

    with %{requirement: %{variants: variants}} <-
           Enum.find(socket.private.data.smart_cell_definitions, &(&1.kind == kind)),
         {:ok, variant} <- Enum.fetch(variants, variant_idx) do
      dependencies = Enum.map(variant.packages, & &1.dependency)
      Session.add_dependencies(socket.assigns.session.pid, dependencies)
    end

    {status, socket} = maybe_reconnect_runtime(socket)

    if status == :ok do
      Session.queue_cell_evaluation(socket.assigns.session.pid, Cell.setup_cell_id())
    end

    {:noreply, socket}
  end

  def handle_event("queue_cell_evaluation", %{"cell_id" => cell_id}, socket) do
    assert_policy!(socket, :execute)
    data = socket.private.data

    {status, socket} =
      with {:ok, cell, _section} <- Notebook.fetch_cell_and_section(data.notebook, cell_id),
           true <- Cell.setup?(cell),
           false <- data.cell_infos[cell.id].eval.validity == :fresh do
        maybe_reconnect_runtime(socket)
      else
        _ -> {:ok, socket}
      end

    if status == :ok do
      Session.queue_cell_evaluation(socket.assigns.session.pid, cell_id)
    end

    {:noreply, socket}
  end

  def handle_event("queue_section_evaluation", %{"section_id" => section_id}, socket) do
    assert_policy!(socket, :execute)
    Session.queue_section_evaluation(socket.assigns.session.pid, section_id)

    {:noreply, socket}
  end

  def handle_event("queue_full_evaluation", %{"forced_cell_ids" => forced_cell_ids}, socket) do
    assert_policy!(socket, :execute)
    Session.queue_full_evaluation(socket.assigns.session.pid, forced_cell_ids)

    {:noreply, socket}
  end

  def handle_event("cancel_cell_evaluation", %{"cell_id" => cell_id}, socket) do
    assert_policy!(socket, :execute)
    Session.cancel_cell_evaluation(socket.assigns.session.pid, cell_id)

    {:noreply, socket}
  end

  def handle_event("queue_cells_reevaluation", %{}, socket) do
    assert_policy!(socket, :execute)
    Session.queue_cells_reevaluation(socket.assigns.session.pid)

    {:noreply, socket}
  end

  def handle_event("save", %{}, socket) do
    assert_policy!(socket, :edit)

    if socket.private.data.file do
      Session.save(socket.assigns.session.pid)
      {:noreply, socket}
    else
      {:noreply,
       push_patch(socket,
         to: Routes.session_path(socket, :file_settings, socket.assigns.session.id)
       )}
    end
  end

  def handle_event("reconnect_runtime", %{}, socket) do
    assert_policy!(socket, :edit)
    {_, socket} = maybe_reconnect_runtime(socket)
    {:noreply, socket}
  end

  def handle_event("connect_runtime", %{}, socket) do
    assert_policy!(socket, :edit)
    {_, socket} = connect_runtime(socket)
    {:noreply, socket}
  end

  def handle_event("setup_default_runtime", %{}, socket) do
    assert_policy!(socket, :edit)
    {status, socket} = connect_runtime(socket)

    if status == :ok do
      Session.queue_cell_evaluation(socket.assigns.session.pid, Cell.setup_cell_id())
    end

    {:noreply, socket}
  end

  def handle_event("disconnect_runtime", %{}, socket) do
    assert_policy!(socket, :edit)
    Session.disconnect_runtime(socket.assigns.session.pid)
    {:noreply, socket}
  end

  def handle_event("intellisense_request", %{"cell_id" => cell_id} = params, socket) do
    assert_policy!(socket, :read)

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

    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(data.notebook, cell_id) do
      if Runtime.connected?(data.runtime) do
        base_locator = Session.find_base_locator(data, cell, section, existing: true)
        ref = Runtime.handle_intellisense(data.runtime, self(), request, base_locator)

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
    assert_policy!(socket, :read)
    %{pid: pid, images_dir: images_dir} = socket.assigns.session
    # Fetch the data, as we don't keep cells' source in the state
    data = Session.get_data(pid)
    notebook = Notebook.forked(data.notebook)
    {:noreply, create_session(socket, notebook: notebook, copy_images_from: images_dir)}
  end

  def handle_event("erase_outputs", %{}, socket) do
    assert_policy!(socket, :execute)
    Session.erase_outputs(socket.assigns.session.pid)
    {:noreply, socket}
  end

  def handle_event("location_report", report, socket) do
    assert_policy!(socket, :read)

    Phoenix.PubSub.broadcast_from(
      Livebook.PubSub,
      self(),
      "sessions:#{socket.assigns.session.id}",
      {:location_report, self(), report}
    )

    {:noreply, socket}
  end

  @impl true
  def handle_info({:operation, operation}, socket) do
    {:noreply, handle_operation(socket, operation)}
  end

  def handle_info({:error, error}, socket) do
    message = error |> to_string() |> upcase_first()

    {:noreply, put_flash(socket, :error, message)}
  end

  def handle_info({:info, info}, socket) do
    message = info |> to_string() |> upcase_first()

    {:noreply, put_flash(socket, :info, message)}
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

  def handle_info({:session_updated, session}, socket) do
    {:noreply, assign(socket, :session, session)}
  end

  def handle_info(:session_closed, socket) do
    {:noreply,
     socket
     |> put_flash(:info, "Session has been closed")
     |> push_redirect(to: Routes.home_path(socket, :page))}
  end

  def handle_info({:runtime_intellisense_response, ref, request, response}, socket) do
    response = process_intellisense_response(response, request)
    payload = %{"ref" => inspect(ref), "response" => response}
    {:noreply, push_event(socket, "intellisense_response", payload)}
  end

  def handle_info({:location_report, client_pid, report}, socket) do
    report = Map.put(report, :client_pid, inspect(client_pid))
    {:noreply, push_event(socket, "location_report", report)}
  end

  def handle_info({:set_input_values, values, local}, socket) do
    if local do
      socket =
        Enum.reduce(values, socket, fn {input_id, value}, socket ->
          operation = {:set_input_value, self(), input_id, value}
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

  def handle_info(_message, socket), do: {:noreply, socket}

  defp handle_relative_path(socket, path) do
    cond do
      String.ends_with?(path, LiveMarkdown.extension()) ->
        handle_relative_notebook_path(socket, path)

      true ->
        socket
        |> put_flash(
          :error,
          "Got unrecognised session path: #{path}\nIf you want to link another notebook, make sure to include the .livemd extension"
        )
        |> redirect_to_self()
    end
  end

  defp handle_relative_notebook_path(socket, relative_path) do
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
        origin = Notebook.ContentLoader.resolve_location(resolution_location, relative_path)

        case session_id_by_location(origin) do
          {:ok, session_id} ->
            push_redirect(socket, to: Routes.session_path(socket, :page, session_id))

          {:error, :none} ->
            open_notebook(socket, origin)

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

  defp open_notebook(socket, origin) do
    case Notebook.ContentLoader.fetch_content_from_location(origin) do
      {:ok, content} ->
        {notebook, messages} = Livebook.LiveMarkdown.notebook_from_livemd(content)

        # If the current session has no path, fork the notebook
        fork? = socket.private.data.file == nil
        {file, notebook} = file_and_notebook(fork?, origin, notebook)

        socket
        |> put_import_warnings(messages)
        |> create_session(notebook: notebook, origin: origin, file: file)

      {:error, message} ->
        socket
        |> put_flash(:error, "Cannot navigate, " <> message)
        |> redirect_to_self()
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
    push_patch(socket, to: Routes.session_path(socket, :page, socket.assigns.session.id))
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

  defp after_operation(socket, _prev_socket, {:client_join, client_pid, user}) do
    push_event(socket, "client_joined", %{client: client_info(client_pid, user)})
  end

  defp after_operation(socket, _prev_socket, {:client_leave, client_pid}) do
    push_event(socket, "client_left", %{client_pid: inspect(client_pid)})
  end

  defp after_operation(socket, _prev_socket, {:update_user, _client_pid, user}) do
    updated_clients =
      socket.private.data.clients_map
      |> Enum.filter(fn {_client_pid, user_id} -> user_id == user.id end)
      |> Enum.map(fn {client_pid, _user_id} -> client_info(client_pid, user) end)

    push_event(socket, "clients_updated", %{clients: updated_clients})
  end

  defp after_operation(socket, _prev_socket, {:set_notebook_name, _client_pid, name}) do
    assign(socket, page_title: get_page_title(name))
  end

  defp after_operation(socket, _prev_socket, {:insert_section, client_pid, _index, section_id}) do
    if client_pid == self() do
      push_event(socket, "section_inserted", %{section_id: section_id})
    else
      socket
    end
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:insert_section_into, client_pid, _section_id, _index, section_id}
       ) do
    if client_pid == self() do
      push_event(socket, "section_inserted", %{section_id: section_id})
    else
      socket
    end
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:delete_section, _client_pid, section_id, _delete_cells}
       ) do
    push_event(socket, "section_deleted", %{section_id: section_id})
  end

  defp after_operation(socket, _prev_socket, {:insert_cell, client_pid, _, _, _, cell_id, _attrs}) do
    socket = prune_cell_sources(socket)

    if client_pid == self() do
      push_event(socket, "cell_inserted", %{cell_id: cell_id})
    else
      socket
    end
  end

  defp after_operation(socket, prev_socket, {:delete_cell, _client_pid, cell_id}) do
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

  defp after_operation(socket, _prev_socket, {:restore_cell, client_pid, cell_id}) do
    socket = prune_cell_sources(socket)

    if client_pid == self() do
      push_event(socket, "cell_restored", %{cell_id: cell_id})
    else
      socket
    end
  end

  defp after_operation(socket, _prev_socket, {:move_cell, client_pid, cell_id, _offset}) do
    if client_pid == self() do
      push_event(socket, "cell_moved", %{cell_id: cell_id})
    else
      socket
    end
  end

  defp after_operation(socket, _prev_socket, {:move_section, client_pid, section_id, _offset}) do
    if client_pid == self() do
      push_event(socket, "section_moved", %{section_id: section_id})
    else
      socket
    end
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:add_cell_evaluation_output, _client_pid, _cell_id, _output}
       ) do
    prune_outputs(socket)
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:add_cell_evaluation_response, _client_pid, cell_id, _output, metadata}
       ) do
    socket
    |> prune_outputs()
    |> push_event("evaluation_finished:#{cell_id}", %{code_error: metadata.code_error})
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:smart_cell_started, _client_pid, _cell_id, _delta, _js_view, _editor}
       ) do
    prune_cell_sources(socket)
  end

  defp after_operation(socket, _prev_socket, _operation), do: socket

  defp handle_actions(socket, actions) do
    Enum.reduce(actions, socket, &handle_action(&2, &1))
  end

  defp handle_action(socket, {:broadcast_delta, client_pid, cell, tag, delta}) do
    if client_pid == self() do
      push_event(socket, "cell_acknowledgement:#{cell.id}:#{tag}", %{})
    else
      push_event(socket, "cell_delta:#{cell.id}:#{tag}", %{delta: Delta.to_compressed(delta)})
    end
  end

  defp handle_action(socket, _action), do: socket

  defp client_info(pid, user) do
    %{pid: inspect(pid), hex_color: user.hex_color, name: user.name || "Anonymous"}
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

  defp cell_type_and_attrs_from_params(%{"type" => "markdown"}), do: {:markdown, %{}}
  defp cell_type_and_attrs_from_params(%{"type" => "code"}), do: {:code, %{}}

  defp cell_type_and_attrs_from_params(%{"type" => "smart", "kind" => kind}) do
    {:smart, %{kind: kind}}
  end

  defp cell_type_and_attrs_from_params(%{"type" => "diagram"}) do
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

  # Builds view-specific structure of data by cherry-picking
  # only the relevant attributes.
  # We then use `@data_view` in the templates and consequently
  # irrelevant changes to data don't change `@data_view`, so LV doesn't
  # have to traverse the whole template tree and no diff is sent to the client.
  defp data_to_view(data) do
    %{
      file: data.file,
      persist_outputs: data.notebook.persist_outputs,
      autosave_interval_s: data.notebook.autosave_interval_s,
      dirty: data.dirty,
      runtime: data.runtime,
      smart_cell_definitions: data.smart_cell_definitions,
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
        |> Enum.map(fn {client_pid, user_id} -> {client_pid, data.users_map[user_id]} end)
        |> Enum.sort_by(fn {_client_pid, user} -> user.name end),
      installing?: data.cell_infos[Cell.setup_cell_id()].eval.status == :evaluating,
      setup_cell_view: %{cell_to_view(hd(data.notebook.setup_section.cells), data) | type: :setup},
      section_views: section_views(data.notebook.sections, data),
      bin_entries: data.bin_entries
    }
  end

  defp cells_status(cells, data) do
    cond do
      evaluating = Enum.find(cells, &evaluating?(&1, data)) ->
        {:evaluating, evaluating.id}

      stale = Enum.find(cells, &stale?(&1, data)) ->
        {:stale, stale.id}

      evaluated = Enum.find(Enum.reverse(cells), &evaluated?(&1, data)) ->
        {:evaluated, evaluated.id}

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

  defp evaluating?(cell, data) do
    get_in(data.cell_infos, [cell.id, :eval, :status]) == :evaluating
  end

  defp stale?(cell, data) do
    get_in(data.cell_infos, [cell.id, :eval, :validity]) == :stale
  end

  defp evaluated?(cell, data) do
    get_in(data.cell_infos, [cell.id, :eval, :validity]) == :evaluated
  end

  defp section_views(sections, data) do
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
        cell_views: Enum.map(section.cells, &cell_to_view(&1, data))
      }
    end)
  end

  defp parent_section_view(nil, _data), do: nil

  defp parent_section_view(parent_id, data) do
    {:ok, section} = Notebook.fetch_section(data.notebook, parent_id)
    %{id: section.id, name: section.name}
  end

  defp cell_to_view(%Cell.Markdown{} = cell, data) do
    info = data.cell_infos[cell.id]

    %{
      id: cell.id,
      type: :markdown,
      source_view: source_view(cell.source, info.sources.primary)
    }
  end

  defp cell_to_view(%Cell.Code{} = cell, data) do
    info = data.cell_infos[cell.id]

    %{
      id: cell.id,
      type: :code,
      source_view: source_view(cell.source, info.sources.primary),
      eval: eval_info_to_view(cell, info.eval, data),
      reevaluate_automatically: cell.reevaluate_automatically
    }
  end

  defp cell_to_view(%Cell.Smart{} = cell, data) do
    info = data.cell_infos[cell.id]

    %{
      id: cell.id,
      type: :smart,
      source_view: source_view(cell.source, info.sources.primary),
      eval: eval_info_to_view(cell, info.eval, data),
      status: info.status,
      js_view: cell.js_view,
      editor:
        cell.editor &&
          %{
            language: cell.editor.language,
            placement: cell.editor.placement,
            source_view: source_view(cell.editor.source, info.sources.secondary)
          }
    }
  end

  defp eval_info_to_view(cell, eval_info, data) do
    %{
      outputs: cell.outputs,
      validity: eval_info.validity,
      status: eval_info.status,
      evaluation_time_ms: eval_info.evaluation_time_ms,
      evaluation_start: eval_info.evaluation_start,
      evaluation_digest: encode_digest(eval_info.evaluation_digest),
      outputs_batch_number: eval_info.outputs_batch_number,
      # Pass input values relevant to the given cell
      input_values: input_values_for_cell(cell, data)
    }
  end

  defp source_view(:__pruned__, _source_info) do
    :__pruned__
  end

  defp source_view(source, source_info) do
    %{
      source: source,
      revision: source_info.revision
    }
  end

  defp input_values_for_cell(cell, data) do
    input_ids =
      for output <- cell.outputs,
          attrs <- Cell.find_inputs_in_output(output),
          do: attrs.id

    Map.take(data.input_values, input_ids)
  end

  # Updates current data_view in response to an operation.
  # In most cases we simply recompute data_view, but for the
  # most common ones we only update the relevant parts.
  defp update_data_view(data_view, prev_data, data, operation) do
    case operation do
      {:report_cell_revision, _pid, _cell_id, _tag, _revision} ->
        data_view

      {:apply_cell_delta, _pid, _cell_id, _tag, _delta, _revision} ->
        update_dirty_status(data_view, data)

      {:update_smart_cell, _pid, _cell_id, _cell_state, _delta, _reevaluate} ->
        update_dirty_status(data_view, data)

      # For outputs that update existing outputs we send the update directly
      # to the corresponding component, so the DOM patch is isolated and fast.
      # This is important for intensive output updates
      {:add_cell_evaluation_output, _client_pid, _cell_id,
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

      {:add_cell_evaluation_output, _client_pid, cell_id, {:stdout, text}} ->
        # Lookup in previous data to see if the output is already there
        case Notebook.fetch_cell_and_section(prev_data.notebook, cell_id) do
          {:ok, %{outputs: [{idx, {:stdout, _}} | _]}, _section} ->
            send_update(LivebookWeb.Output.StdoutComponent, id: "output-#{idx}", text: text)
            data_view

          _ ->
            data_to_view(data)
        end

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
    "Livebook - #{notebook_name}"
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

  defp assert_policy!(socket, key) do
    unless socket.assigns.policy |> Map.fetch!(key) do
      raise "policy not allowed"
    end

    :ok
  end
end

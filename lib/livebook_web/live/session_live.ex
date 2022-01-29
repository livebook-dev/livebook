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
            Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")

            data
          else
            Session.get_data(session_pid)
          end

        session = Session.get_by_pid(session_pid)

        platform = platform_from_socket(socket)

        {:ok,
         socket
         |> assign(
           session: session,
           platform: platform,
           self: self(),
           data_view: data_to_view(data),
           autofocus_cell_id: autofocus_cell_id(data.notebook),
           page_title: get_page_title(data.notebook.name),
           empty_default_runtime: Livebook.Config.default_runtime() |> elem(0) |> struct()
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
    <div class="flex grow h-full"
      id={"session-#{@session.id}"}
      data-element="session"
      phx-hook="Session"
      data-global-status={elem(@data_view.global_status, 0)}
      data-autofocus-cell-id={@autofocus_cell_id}>
      <SidebarHelpers.sidebar>
        <SidebarHelpers.logo_item socket={@socket} />
        <SidebarHelpers.button_item
          icon="booklet-fill"
          label="Sections (ss)"
          data_element="sections-list-toggle" />
        <SidebarHelpers.button_item
          icon="group-fill"
          label="Connected users (su)"
          data_element="clients-list-toggle" />
        <SidebarHelpers.button_item
          icon="cpu-line"
          label="Runtime settings (sr)"
          data_element="runtime-info-toggle" />
        <SidebarHelpers.link_item
          icon="delete-bin-6-fill"
          label="Bin (sb)"
          path={Routes.session_path(@socket, :bin, @session.id)}
          active={@live_action == :bin} />
        <SidebarHelpers.break_item />
        <SidebarHelpers.link_item
          icon="keyboard-box-fill"
          label="Keyboard shortcuts (?)"
          path={Routes.session_path(@socket, :shortcuts, @session.id)}
          active={@live_action == :shortcuts} />
        <SidebarHelpers.user_item
          current_user={@current_user}
          path={Routes.session_path(@socket, :user, @session.id)} />
      </SidebarHelpers.sidebar>
      <div class="flex flex-col h-full w-full max-w-xs absolute z-30 top-0 left-[64px] overflow-y-auto shadow-xl md:static md:shadow-none bg-gray-50 border-r border-gray-100 px-6 py-10"
        data-element="side-panel">
        <div data-element="sections-list">
          <.sections_list data_view={@data_view} />
        </div>
        <div data-element="clients-list">
          <.clients_list data_view={@data_view} self={@self} />
        </div>
        <div data-element="runtime-info">
          <.runtime_info data_view={@data_view} session={@session} socket={@socket} empty_default_runtime={@empty_default_runtime} />
        </div>
      </div>
      <div class="grow overflow-y-auto relative" data-element="notebook">
        <div data-element="output-iframes" phx-update="ignore" id="output-iframes"></div>
        <div class="w-full max-w-screen-lg px-16 mx-auto py-7" data-element="notebook-content">
          <div class="flex items-center pb-4 mb-6 space-x-4 border-b border-gray-200"
            data-element="notebook-headline"
            data-focusable-id="notebook"
            id="notebook"
            phx-hook="Headline"
            data-on-value-change="set_notebook_name"
            data-metadata="notebook">
            <h1 class="grow p-1 -ml-1 text-3xl font-semibold text-gray-800 border border-transparent rounded-lg whitespace-pre-wrap"
              tabindex="0"
              id="notebook-heading"
              data-element="heading"
              spellcheck="false"><%= @data_view.notebook_name %></h1>
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
                <button class="menu-item text-gray-500"
                  role="menuitem"
                  phx-click="erase_outputs">
                  <.remix_icon icon="eraser-fill" />
                  <span class="font-medium">Erase outputs</span>
                </button>
                <button class="menu-item text-gray-500"
                  role="menuitem"
                  phx-click="fork_session">
                  <.remix_icon icon="git-branch-line" />
                  <span class="font-medium">Fork</span>
                </button>
                <a class="menu-item text-gray-500"
                  role="menuitem"
                  href={live_dashboard_process_path(@socket, @session.pid)}
                  target="_blank">
                  <.remix_icon icon="dashboard-2-line" />
                  <span class="font-medium">See on Dashboard</span>
                </a>
                <%= live_patch to: Routes.home_path(@socket, :close_session, @session.id),
                      class: "menu-item text-red-600",
                      role: "menuitem" do %>
                  <.remix_icon icon="close-circle-line" />
                  <span class="font-medium">Close</span>
                <% end %>
              </:content>
            </.menu>
          </div>
          <div class="flex flex-col w-full space-y-16">
            <%= if @data_view.section_views == [] do %>
              <div class="flex justify-center">
                <button class="button-base button-small"
                  phx-click="append_section">
                  + Section
                </button>
              </div>
            <% end %>
            <%= for {section_view, index} <- Enum.with_index(@data_view.section_views) do %>
              <.live_component module={LivebookWeb.SessionLive.SectionComponent}
                  id={section_view.id}
                  index={index}
                  session_id={@session.id}
                  runtime={@data_view.runtime}
                  section_view={section_view} />
            <% end %>
            <div style="height: 80vh"></div>
          </div>
        </div>
      </div>
      <div class="fixed bottom-[0.4rem] right-[1.5rem]">
        <LivebookWeb.SessionLive.IndicatorsComponent.render
          socket={@socket}
          session_id={@session.id}
          file={@data_view.file}
          dirty={@data_view.dirty}
          autosave_interval_s={@data_view.autosave_interval_s}
          runtime={@data_view.runtime}
          global_status={@data_view.global_status} />
      </div>
    </div>

    <%= if @live_action == :user do %>
      <.current_user_modal
        return_to={Routes.session_path(@socket, :page, @session.id)}
        current_user={@current_user} />
    <% end %>

    <%= if @live_action == :runtime_settings do %>
      <.modal class="w-full max-w-4xl" return_to={Routes.session_path(@socket, :page, @session.id)}>
        <.live_component module={LivebookWeb.SessionLive.RuntimeComponent}
          id="runtime-settings"
          session={@session}
          runtime={@data_view.runtime} />
      </.modal>
    <% end %>

    <%= if @live_action == :file_settings do %>
      <.modal class="w-full max-w-4xl" return_to={Routes.session_path(@socket, :page, @session.id)}>
        <%= live_render @socket, LivebookWeb.SessionLive.PersistenceLive,
              id: "persistence",
              session: %{
                "session" => @session,
                "file" => @data_view.file,
                "persist_outputs" => @data_view.persist_outputs,
                "autosave_interval_s" => @data_view.autosave_interval_s
              } %>
      </.modal>
    <% end %>

    <%= if @live_action == :shortcuts do %>
      <.modal class="w-full max-w-6xl" return_to={Routes.session_path(@socket, :page, @session.id)}>
        <.live_component module={LivebookWeb.SessionLive.ShortcutsComponent}
          id="shortcuts"
          platform={@platform} />
      </.modal>
    <% end %>

    <%= if @live_action == :cell_settings do %>
      <.modal class="w-full max-w-xl" return_to={Routes.session_path(@socket, :page, @session.id)}>
        <.live_component module={settings_component_for(@cell)}
          id="cell-settings"
          session={@session}
          return_to={Routes.session_path(@socket, :page, @session.id)}
          cell={@cell} />
      </.modal>
    <% end %>

    <%= if @live_action == :cell_upload do %>
      <.modal class="w-full max-w-xl" return_to={Routes.session_path(@socket, :page, @session.id)}>
        <.live_component module={LivebookWeb.SessionLive.CellUploadComponent}
          id="cell-upload"
          session={@session}
          return_to={Routes.session_path(@socket, :page, @session.id)}
          cell={@cell}
          uploads={@uploads} />
      </.modal>
    <% end %>

    <%= if @live_action == :delete_section do %>
      <.modal class="w-full max-w-xl" return_to={Routes.session_path(@socket, :page, @session.id)}>
        <.live_component module={LivebookWeb.SessionLive.DeleteSectionComponent}
          id="delete-section"
          session={@session}
          return_to={Routes.session_path(@socket, :page, @session.id)}
          section={@section}
          is_first={@section.id == @first_section_id} />
      </.modal>
    <% end %>

    <%= if @live_action == :bin do %>
      <.modal class="w-full max-w-4xl" return_to={Routes.session_path(@socket, :page, @session.id)}>
        <.live_component module={LivebookWeb.SessionLive.BinComponent}
          id="bin"
          session={@session}
          return_to={Routes.session_path(@socket, :page, @session.id)}
          bin_entries={@data_view.bin_entries} />
      </.modal>
    <% end %>

    <%= if @live_action == :export do %>
      <.modal class="w-full max-w-4xl" return_to={Routes.session_path(@socket, :page, @session.id)}>
        <.live_component module={LivebookWeb.SessionLive.ExportComponent}
          id="export"
          session={@session}
          tab={@tab} />
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
            <button class="grow flex items-center text-gray-500 hover:text-gray-900 text-left"
              data-element="sections-list-item"
              data-section-id={section_item.id}>
              <span class="flex items-center space-x-1">
                <span><%= section_item.name %></span>
                <%= if section_item.parent do %>
                  <%# Note: the container has overflow-y auto, so we cannot set overflow-x visible,
                      consequently we show the tooltip wrapped to a fixed number of characters %>
                  <span {branching_tooltip_attrs(section_item.name, section_item.parent.name)}>
                    <.remix_icon icon="git-branch-line" class="text-lg font-normal leading-none flip-horizontally" />
                  </span>
                <% end %>
              </span>
            </button>
            <.session_status status={elem(section_item.status, 0)} cell_id={elem(section_item.status, 1)} />
          </div>
        <% end %>
      </div>
      <button class="inline-flex items-center justify-center p-8 py-1 mt-8 space-x-2 text-sm font-medium text-gray-500 border border-gray-400 border-dashed rounded-xl hover:bg-gray-100"
        phx-click="append_section">
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
          <div class="flex items-center justify-between space-x-2"
            id={"clients-list-item-#{inspect(client_pid)}"}
            data-element="clients-list-item"
            data-client-pid={inspect(client_pid)}>
            <button class="flex items-center space-x-2 text-gray-500 hover:text-gray-900 disabled:pointer-events-none"
              disabled={client_pid == @self}
              data-element="client-link">
              <.user_avatar user={user} class="shrink-0 h-7 w-7" text_class="text-xs" />
              <span><%= user.name || "Anonymous" %></span>
            </button>
            <%= if client_pid != @self do %>
              <span class="tooltip left" data-tooltip="Follow this user"
                data-element="client-follow-toggle"
                data-meta="follow">
                <button class="icon-button" aria-label="follow this user">
                  <.remix_icon icon="pushpin-line" class="text-lg" />
                </button>
              </span>
              <span class="tooltip left" data-tooltip="Unfollow this user"
                data-element="client-follow-toggle"
                data-meta="unfollow">
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
              class: "icon-button" do  %>
          <.remix_icon icon="settings-3-line text-xl" />
        <% end %>
      </div>
      <div class="flex flex-col mt-2 space-y-4">
        <%= if @data_view.runtime do %>
          <div class="flex flex-col space-y-3">
            <.labeled_text label="Type" text={runtime_type_label(@data_view.runtime)} />
            <.labeled_text label="Node name" text={@data_view.runtime.node} one_line={true} />
          </div>
          <div class="flex flex-col space-y-3">
            <div class="flex space-x-2">
              <button class="button-base button-blue" phx-click="restart_runtime">
                <.remix_icon icon="wireless-charging-line" class="align-middle mr-1" />
                <span>Reconnect</span>
              </button>
              <button class="button-base button-outlined-red"
                type="button"
                phx-click="disconnect_runtime">
                Disconnect
              </button>
            </div>
          </div>
        <% else %>
          <div class="flex flex-col space-y-3">
            <.labeled_text label="Type" text={runtime_type_label(@empty_default_runtime)} />
          </div>
          <div class="flex space-x-2">
            <button class="button-base button-blue" phx-click="connect_default_runtime">
              <.remix_icon icon="wireless-charging-line" class="align-middle mr-1" />
              <span>Connect</span>
            </button>
            <%= live_patch to: Routes.session_path(@socket, :runtime_settings, @session.id),
                  class: "button-base button-outlined-gray bg-transparent" do  %>
              Configure
            <% end %>
          </div>
        <% end %>
        <%= if uses_memory?(@session.memory_usage) do %>
          <.memory_info memory_usage={@session.memory_usage} />
        <% else %>
          <div class="mb-1 text-sm text-gray-800 py-6 flex flex-col">
            <span class="w-full uppercase font-semibold text-gray-500">Memory</span>
            <p class="py-1">
              <%= format_bytes(@session.memory_usage.system.free) %>
              available out of
              <%= format_bytes(@session.memory_usage.system.total) %>
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

  defp runtime_type_label(%Runtime.ElixirStandalone{}), do: "Elixir standalone"
  defp runtime_type_label(%Runtime.MixStandalone{}), do: "Mix standalone"
  defp runtime_type_label(%Runtime.Attached{}), do: "Attached"
  defp runtime_type_label(%Runtime.Embedded{}), do: "Embedded"

  defp session_status(%{status: :evaluating} = assigns) do
    ~H"""
    <button data-element="focus-cell-button" data-target={@cell_id}>
      <.status_indicator circle_class="bg-blue-500" animated_circle_class="bg-blue-400">
      </.status_indicator>
    </button>
    """
  end

  defp session_status(%{status: :stale} = assigns) do
    ~H"""
    <button data-element="focus-cell-button" data-target={@cell_id}>
      <.status_indicator circle_class="bg-yellow-bright-200">
      </.status_indicator>
    </button>
    """
  end

  defp session_status(assigns), do: ~H""

  defp status_indicator(assigns) do
    assigns = assign_new(assigns, :animated_circle_class, fn -> nil end)

    ~H"""
    <div class="flex items-center space-x-1">
      <span class="flex relative h-3 w-3">
        <%= if @animated_circle_class do %>
          <span class={"#{@animated_circle_class} animate-ping absolute inline-flex h-3 w-3 rounded-full opacity-75"}></span>
        <% end %>
        <span class={"#{@circle_class} relative inline-flex rounded-full h-3 w-3"}></span>
      </span>
    </div>
    """
  end

  defp settings_component_for(%Cell.Elixir{}),
    do: LivebookWeb.SessionLive.ElixirCellSettingsComponent

  defp branching_tooltip_attrs(name, parent_name) do
    direction = if String.length(name) >= 16, do: "left", else: "right"

    wrapped_name = Livebook.Utils.wrap_line("”" <> parent_name <> "”", 16)
    label = "Branches from\n#{wrapped_name}"

    [class: "tooltip #{direction}", data_tooltip: label]
  end

  @impl true
  def handle_params(%{"cell_id" => cell_id}, _url, socket) do
    {:ok, cell, _} = Notebook.fetch_cell_and_section(socket.private.data.notebook, cell_id)
    {:noreply, assign(socket, cell: cell)}
  end

  def handle_params(%{"section_id" => section_id}, _url, socket) do
    {:ok, section} = Notebook.fetch_section(socket.private.data.notebook, section_id)
    first_section_id = hd(socket.private.data.notebook.sections).id
    {:noreply, assign(socket, section: section, first_section_id: first_section_id)}
  end

  def handle_params(
        %{"path_parts" => path_parts},
        _url,
        %{assigns: %{live_action: :catch_all}} = socket
      ) do
    path_parts =
      Enum.map(path_parts, fn
        "__parent__" -> ".."
        part -> part
      end)

    path = Path.join(path_parts)
    {:noreply, handle_relative_path(socket, path)}
  end

  def handle_params(%{"tab" => tab}, _url, socket) do
    {:noreply, assign(socket, tab: tab)}
  end

  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("session_init", _params, socket) do
    data = socket.private.data

    payload = %{
      clients:
        Enum.map(data.clients_map, fn {client_pid, user_id} ->
          client_info(client_pid, data.users_map[user_id])
        end)
    }

    {:reply, payload, socket}
  end

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

  def handle_event("insert_cell_below", %{"type" => type} = params, socket) do
    type = String.to_atom(type)

    with {:ok, section, index} <-
           section_with_next_index(
             socket.private.data.notebook,
             params["section_id"],
             params["cell_id"]
           ) do
      Session.insert_cell(socket.assigns.session.pid, section.id, index, type)
    end

    {:noreply, socket}
  end

  def handle_event("delete_cell", %{"cell_id" => cell_id}, socket) do
    Session.delete_cell(socket.assigns.session.pid, cell_id)

    {:noreply, socket}
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
        %{"cell_id" => cell_id, "delta" => delta, "revision" => revision},
        socket
      ) do
    delta = Delta.from_compressed(delta)
    Session.apply_cell_delta(socket.assigns.session.pid, cell_id, delta, revision)

    {:noreply, socket}
  end

  def handle_event(
        "report_cell_revision",
        %{"cell_id" => cell_id, "revision" => revision},
        socket
      ) do
    Session.report_cell_revision(socket.assigns.session.pid, cell_id, revision)

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

  def handle_event("queue_cell_evaluation", %{"cell_id" => cell_id}, socket) do
    Session.queue_cell_evaluation(socket.assigns.session.pid, cell_id)

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

  def handle_event("save", %{}, socket) do
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

  def handle_event("show_shortcuts", %{}, socket) do
    {:noreply,
     push_patch(socket, to: Routes.session_path(socket, :shortcuts, socket.assigns.session.id))}
  end

  def handle_event("show_bin", %{}, socket) do
    {:noreply,
     push_patch(socket, to: Routes.session_path(socket, :bin, socket.assigns.session.id))}
  end

  def handle_event("restart_runtime", %{}, socket) do
    socket =
      if runtime = socket.private.data.runtime do
        case Runtime.duplicate(runtime) do
          {:ok, new_runtime} ->
            Session.connect_runtime(socket.assigns.session.pid, new_runtime)
            clear_flash(socket, :error)

          {:error, message} ->
            put_flash(socket, :error, "Failed to setup runtime - #{message}")
        end
      else
        socket
      end

    {:noreply, socket}
  end

  def handle_event("connect_default_runtime", %{}, socket) do
    {runtime_module, args} = Livebook.Config.default_runtime()

    socket =
      case apply(runtime_module, :init, args) do
        {:ok, runtime} ->
          Session.connect_runtime(socket.assigns.session.pid, runtime)
          socket

        {:error, message} ->
          put_flash(socket, :error, "Failed to setup runtime - #{message}")
      end

    {:noreply, socket}
  end

  def handle_event("disconnect_runtime", %{}, socket) do
    Session.disconnect_runtime(socket.assigns.session.pid)
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

    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(data.notebook, cell_id) do
      if data.runtime do
        ref = make_ref()
        prev_locator = Session.find_prev_locator(data.notebook, cell, section)
        Runtime.handle_intellisense(data.runtime, self(), ref, request, prev_locator)

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
    %{pid: pid, images_dir: images_dir} = socket.assigns.session
    # Fetch the data, as we don't keep cells' source in the state
    data = Session.get_data(pid)
    notebook = Notebook.forked(data.notebook)
    {:noreply, create_session(socket, notebook: notebook, copy_images_from: images_dir)}
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
      {:location_report, self(), report}
    )

    {:noreply, socket}
  end

  def handle_event("format_code", %{"code" => code}, socket) do
    formatted =
      try do
        code
        |> Code.format_string!()
        |> IO.iodata_to_binary()
      rescue
        _ -> code
      end

    {:reply, %{code: formatted}, socket}
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

  def handle_info({:intellisense_response, ref, request, response}, socket) do
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
        origin = Livebook.ContentLoader.resolve_location(resolution_location, relative_path)

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
    case Livebook.ContentLoader.fetch_content_from_location(origin) do
      {:ok, content} ->
        {notebook, messages} = Livebook.LiveMarkdown.Import.notebook_from_markdown(content)

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

  defp after_operation(socket, _prev_socket, {:insert_cell, client_pid, _, _, _, cell_id}) do
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
         {:evaluation_started, _client_pid, cell_id, evaluation_digest}
       ) do
    push_event(socket, "evaluation_started:#{cell_id}", %{
      evaluation_digest: encode_digest(evaluation_digest)
    })
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

  defp after_operation(socket, _prev_socket, _operation), do: socket

  defp handle_actions(socket, actions) do
    Enum.reduce(actions, socket, &handle_action(&2, &1))
  end

  defp handle_action(socket, {:broadcast_delta, client_pid, cell, delta}) do
    if client_pid == self() do
      push_event(socket, "cell_acknowledgement:#{cell.id}", %{})
    else
      push_event(socket, "cell_delta:#{cell.id}", %{delta: Delta.to_compressed(delta)})
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
      section_views: section_views(data.notebook.sections, data),
      bin_entries: data.bin_entries,
      created_at: DateTime.now!("Etc/UTC")
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
      |> Notebook.elixir_cells_with_section()
      |> Enum.map(fn {cell, _} -> cell end)

    cells_status(cells, data)
  end

  defp evaluating?(cell, data), do: data.cell_infos[cell.id].evaluation_status == :evaluating

  defp stale?(cell, data), do: data.cell_infos[cell.id].validity_status == :stale

  defp evaluated?(cell, data), do: data.cell_infos[cell.id].validity_status == :evaluated

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

  defp cell_to_view(%Cell.Elixir{} = cell, data) do
    info = data.cell_infos[cell.id]

    %{
      id: cell.id,
      type: :elixir,
      source_info: cell_source_info(cell, info),
      outputs: cell.outputs,
      validity_status: info.validity_status,
      evaluation_status: info.evaluation_status,
      evaluation_time_ms: info.evaluation_time_ms,
      evaluation_start: info.evaluation_start,
      evaluation_number: info.evaluation_number,
      outputs_batch_number: info.outputs_batch_number,
      reevaluate_automatically: cell.reevaluate_automatically,
      # Pass input values relevant to the given cell
      input_values: input_values_for_cell(cell, data)
    }
  end

  defp cell_to_view(%Cell.Markdown{} = cell, data) do
    info = data.cell_infos[cell.id]

    %{
      id: cell.id,
      type: :markdown,
      source_info: cell_source_info(cell, info)
    }
  end

  defp cell_source_info(%{source: :__pruned__}, _info) do
    :__pruned__
  end

  defp cell_source_info(cell, info) do
    %{
      source: cell.source,
      revision: info.revision,
      evaluation_digest: encode_digest(info.evaluation_digest)
    }
  end

  defp input_values_for_cell(cell, data) do
    input_ids =
      for output <- cell.outputs,
          attrs <- Cell.Elixir.find_inputs_in_output(output),
          do: attrs.id

    Map.take(data.input_values, input_ids)
  end

  # Updates current data_view in response to an operation.
  # In most cases we simply recompute data_view, but for the
  # most common ones we only update the relevant parts.
  defp update_data_view(data_view, prev_data, data, operation) do
    case operation do
      {:report_cell_revision, _pid, _cell_id, _revision} ->
        data_view

      {:apply_cell_delta, _pid, _cell_id, _delta, _revision} ->
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
            %{source: _} = cell -> %{cell | source: :__pruned__}
            cell -> cell
          end)
        )
    )
  end

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
end

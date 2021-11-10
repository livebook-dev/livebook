defmodule LivebookWeb.SessionLive do
  use LivebookWeb, :live_view

  import LivebookWeb.UserHelpers
  import LivebookWeb.SessionHelpers
  import Livebook.Utils, only: [access_by_id: 1]

  alias LivebookWeb.SidebarHelpers
  alias Livebook.{Sessions, Session, Delta, Notebook, Runtime, LiveMarkdown, FileSystem}
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
           empty_default_runtime: Livebook.Config.default_runtime() |> elem(0) |> struct()
         )
         |> assign_private(data: data)
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
    with connect_info when connect_info != nil <- get_connect_info(socket),
         {:ok, user_agent} <- Map.fetch(connect_info, :user_agent) do
      platform_from_user_agent(user_agent)
    else
      _ -> nil
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-grow h-full"
      id="session"
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
      <div class="flex-grow overflow-y-auto scroll-smooth" data-element="notebook">
        <div class="w-full max-w-screen-lg px-16 mx-auto py-7">
          <div class="flex items-center pb-4 mb-6 space-x-4 border-b border-gray-200">
            <h1 class="flex-grow p-1 -ml-1 text-3xl font-semibold text-gray-800 border border-transparent rounded-lg hover:border-blue-200 focus:border-blue-300"
              aria-description="notebook title"
              id="notebook-name"
              data-element="notebook-name"
              contenteditable
              spellcheck="false"
              phx-blur="set_notebook_name"
              phx-hook="ContentEditable"
              data-update-attribute="phx-value-name"><%= @data_view.notebook_name %></h1>
            <div class="relative" id="session-menu" phx-hook="Menu" data-element="menu">
              <button class="icon-button" data-toggle>
                <.remix_icon icon="more-2-fill" class="text-xl" />
              </button>
              <div class="menu" data-content>
                <%= live_patch to: Routes.session_path(@socket, :export, @session.id, "livemd"),
                      class: "menu__item text-gray-500" do %>
                  <.remix_icon icon="download-2-line" />
                  <span class="font-medium">Export</span>
                <% end %>
                <button class="text-gray-500 menu__item"
                  phx-click="erase_outputs">
                  <.remix_icon icon="eraser-fill" />
                  <span class="font-medium">Erase outputs</span>
                </button>
                <button class="text-gray-500 menu__item"
                  phx-click="fork_session">
                  <.remix_icon icon="git-branch-line" />
                  <span class="font-medium">Fork</span>
                </button>
                <a class="text-gray-500 menu__item"
                  href={live_dashboard_process_path(@socket, @session.pid)}
                  target="_blank">
                  <.remix_icon icon="dashboard-2-line" />
                  <span class="font-medium">See on Dashboard</span>
                </a>
                <%= live_patch to: Routes.home_path(@socket, :close_session, @session.id),
                      class: "menu__item text-red-600" do %>
                  <.remix_icon icon="close-circle-line" />
                  <span class="font-medium">Close</span>
                <% end %>
              </div>
            </div>
          </div>
          <div class="flex flex-col w-full space-y-16">
            <%= if @data_view.section_views == [] do %>
              <div class="flex justify-center">
                <button class="button button-small"
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
    <div class="flex flex-col flex-grow">
      <h3 class="uppercase text-sm font-semibold text-gray-500">
        Sections
      </h3>
      <div class="flex flex-col mt-4 space-y-4">
        <%= for section_item <- @data_view.sections_items do %>
          <div class="flex items-center">
            <button class="flex-grow flex items-center text-gray-500 hover:text-gray-900"
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
    <div class="flex flex-col flex-grow">
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
              <.user_avatar user={user} class="flex-shrink-0 h-7 w-7" text_class="text-xs" />
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
    <div class="flex flex-col flex-grow">
      <div class="flex items-center justify-between">
        <h3 class="uppercase text-sm font-semibold text-gray-500">
          Runtime
        </h3>
        <%= live_patch to: Routes.session_path(@socket, :runtime_settings, @session.id),
              class: "icon-button",
              type: "button" do  %>
          <.remix_icon icon="settings-3-line text-xl" />
        <% end %>
      </div>
      <div class="flex flex-col mt-4 space-y-4">
        <%= if @data_view.runtime do %>
          <div class="flex flex-col space-y-3">
            <.labeled_text label="Type" text={runtime_type_label(@data_view.runtime)} />
            <.labeled_text label="Node name" text={@data_view.runtime.node} one_line={true} />
          </div>
          <div class="flex flex-col space-y-3">
            <div class="flex space-x-2">
              <button class="button button-blue" phx-click="restart_runtime">
                <.remix_icon icon="wireless-charging-line" class="align-middle mr-1" />
                <span>Reconnect</span>
              </button>
              <button class="button button-outlined-red"
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
            <button class="button button-blue" phx-click="connect_default_runtime">
              <.remix_icon icon="wireless-charging-line" class="align-middle mr-1" />
              <span>Connect</span>
            </button>
            <%= live_patch to: Routes.session_path(@socket, :runtime_settings, @session.id),
                  class: "button button-outlined-gray bg-transparent",
                  type: "button" do  %>
              Configure
            <% end %>
          </div>
        <% end %>
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
      <.status_indicator circle_class="bg-yellow-200">
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

  defp settings_component_for(%Cell.Input{}),
    do: LivebookWeb.SessionLive.InputCellSettingsComponent

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

  def handle_event("cell_init", %{"cell_id" => cell_id}, socket) do
    data = socket.private.data

    case Notebook.fetch_cell_and_section(data.notebook, cell_id) do
      {:ok, cell, _section} ->
        info = data.cell_infos[cell.id]

        payload = %{
          source: cell.source,
          revision: info.revision,
          evaluation_digest: encode_digest(info.evaluation_digest)
        }

        # From this point on we don't need cell source in the LV,
        # so we are going to drop it altogether
        socket = remove_cell_source(socket, cell_id)

        {:reply, payload, socket}

      :error ->
        {:noreply, socket}
    end
  end

  def handle_event("append_section", %{}, socket) do
    idx = length(socket.private.data.notebook.sections)
    Session.insert_section(socket.assigns.session.pid, idx)

    {:noreply, socket}
  end

  def handle_event("insert_section_into", %{"section_id" => section_id, "index" => index}, socket) do
    index = ensure_integer(index) |> max(0)
    Session.insert_section_into(socket.assigns.session.pid, section_id, index)

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

  def handle_event(
        "insert_cell",
        %{"section_id" => section_id, "index" => index, "type" => type},
        socket
      ) do
    index = ensure_integer(index) |> max(0)
    type = String.to_atom(type)
    Session.insert_cell(socket.assigns.session.pid, section_id, index, type)

    {:noreply, socket}
  end

  def handle_event("insert_cell_below", %{"cell_id" => cell_id, "type" => type}, socket) do
    type = String.to_atom(type)
    insert_cell_next_to(socket, cell_id, type, idx_offset: 1)

    {:noreply, socket}
  end

  def handle_event("insert_cell_above", %{"cell_id" => cell_id, "type" => type}, socket) do
    type = String.to_atom(type)
    insert_cell_next_to(socket, cell_id, type, idx_offset: 0)

    {:noreply, socket}
  end

  def handle_event("delete_cell", %{"cell_id" => cell_id}, socket) do
    Session.delete_cell(socket.assigns.session.pid, cell_id)

    {:noreply, socket}
  end

  def handle_event("set_notebook_name", %{"name" => name}, socket) do
    name = normalize_name(name)
    Session.set_notebook_name(socket.assigns.session.pid, name)

    {:noreply, socket}
  end

  def handle_event("set_section_name", %{"section_id" => section_id, "name" => name}, socket) do
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

  def handle_event("set_cell_value", %{"cell_id" => cell_id, "value" => value}, socket) do
    # The browser may normalize newlines to \r\n, but we want \n
    # to more closely imitate an actual shell
    value = String.replace(value, "\r\n", "\n")

    Session.set_cell_attributes(socket.assigns.session.pid, cell_id, %{value: value})

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

  def handle_event("queue_cell_evaluation", %{"cell_id" => cell_id}, socket) do
    Session.queue_cell_evaluation(socket.assigns.session.pid, cell_id)
    {:noreply, socket}
  end

  def handle_event("queue_section_cells_evaluation", %{"section_id" => section_id}, socket) do
    with {:ok, section} <- Notebook.fetch_section(socket.private.data.notebook, section_id) do
      for cell <- section.cells, is_struct(cell, Cell.Elixir) do
        Session.queue_cell_evaluation(socket.assigns.session.pid, cell.id)
      end
    end

    {:noreply, socket}
  end

  def handle_event("queue_all_cells_evaluation", _params, socket) do
    data = socket.private.data

    for {cell, _} <- Notebook.elixir_cells_with_section(data.notebook),
        data.cell_infos[cell.id].validity_status != :evaluated do
      Session.queue_cell_evaluation(socket.assigns.session.pid, cell.id)
    end

    {:noreply, socket}
  end

  def handle_event("queue_bound_cells_evaluation", %{"cell_id" => cell_id}, socket) do
    data = socket.private.data

    with {:ok, cell, _section} <- Notebook.fetch_cell_and_section(data.notebook, cell_id) do
      for {bound_cell, _} <- Session.Data.bound_cells_with_section(data, cell.id) do
        Session.queue_cell_evaluation(socket.assigns.session.pid, bound_cell.id)
      end
    end

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
            socket

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
          case params["type"] do
            "completion" ->
              "You need to start a runtime (or evaluate a cell) for code completion"

            "format" ->
              "You need to start a runtime (or evaluate a cell) to enable code formatting"

            _ ->
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
    case Session.Data.apply_operation(socket.private.data, operation) do
      {:ok, data, actions} ->
        new_socket =
          socket
          |> assign_private(data: data)
          |> assign(data_view: update_data_view(socket.assigns.data_view, data, operation))
          |> after_operation(socket, operation)
          |> handle_actions(actions)

        {:noreply, new_socket}

      :error ->
        {:noreply, socket}
    end
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
        origin =
          case resolution_location do
            {:url, url} -> {:url, Livebook.Utils.expand_url(url, relative_path)}
            {:file, file} -> {:file, FileSystem.File.resolve(file, relative_path)}
          end

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
    case load_content(origin) do
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

  defp load_content({:file, file}) do
    case FileSystem.File.read(file) do
      {:ok, content} -> {:ok, content}
      {:error, message} -> {:error, "failed to read #{file.path}, reason: #{message}"}
    end
  end

  defp load_content({:url, url}) do
    url
    |> Livebook.ContentLoader.rewrite_url()
    |> Livebook.ContentLoader.fetch_content()
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

  defp after_operation(socket, _prev_socket, {:insert_cell, client_pid, _, _, type, cell_id}) do
    if client_pid == self() do
      case type do
        :input ->
          push_patch(socket,
            to: Routes.session_path(socket, :cell_settings, socket.assigns.session.id, cell_id)
          )

        _ ->
          socket
      end
      |> push_event("cell_inserted", %{cell_id: cell_id})
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

  defp insert_cell_next_to(socket, cell_id, type, idx_offset: idx_offset) do
    {:ok, cell, section} = Notebook.fetch_cell_and_section(socket.private.data.notebook, cell_id)
    index = Enum.find_index(section.cells, &(&1 == cell))
    Session.insert_cell(socket.assigns.session.pid, section.id, index + idx_offset, type)
  end

  defp ensure_integer(n) when is_integer(n), do: n
  defp ensure_integer(n) when is_binary(n), do: String.to_integer(n)

  defp encode_digest(nil), do: nil
  defp encode_digest(digest), do: Base.encode64(digest)

  defp remove_cell_source(socket, cell_id) do
    update_in(socket.private.data.notebook, fn notebook ->
      Notebook.update_cell(notebook, cell_id, &%{&1 | source: nil})
    end)
  end

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
      # Note: we need this during initial loading,
      # at which point we still have the source
      empty?: cell.source == "",
      outputs: cell.outputs,
      validity_status: info.validity_status,
      evaluation_status: info.evaluation_status,
      evaluation_time_ms: info.evaluation_time_ms,
      number_of_evaluations: info.number_of_evaluations,
      reevaluate_automatically: cell.reevaluate_automatically
    }
  end

  defp cell_to_view(%Cell.Markdown{} = cell, _data) do
    %{
      id: cell.id,
      type: :markdown,
      # Note: we need this during initial loading,
      # at which point we still have the source
      empty?: cell.source == ""
    }
  end

  defp cell_to_view(%Cell.Input{} = cell, _data) do
    %{
      id: cell.id,
      type: :input,
      input_type: cell.type,
      name: cell.name,
      value: cell.value,
      error:
        case Cell.Input.validate(cell) do
          :ok -> nil
          {:error, error} -> error
        end,
      props: cell.props
    }
  end

  # Updates current data_view in response to an operation.
  # In most cases we simply recompute data_view, but for the
  # most common ones we only update the relevant parts.
  defp update_data_view(data_view, data, operation) do
    case operation do
      {:report_cell_revision, _pid, _cell_id, _revision} ->
        data_view

      {:apply_cell_delta, _pid, cell_id, _delta, _revision} ->
        data_view
        |> update_cell_view(data, cell_id)
        |> update_dirty_status(data)

      _ ->
        data_to_view(data)
    end
  end

  defp update_cell_view(data_view, data, cell_id) do
    {:ok, cell, section} = Notebook.fetch_cell_and_section(data.notebook, cell_id)
    cell_view = cell_to_view(cell, data)

    put_in(
      data_view,
      [:section_views, access_by_id(section.id), :cell_views, access_by_id(cell.id)],
      cell_view
    )
  end

  # Changes that affect only a single cell are still likely to
  # have impact on dirtiness, so we need to always mirror it
  defp update_dirty_status(data_view, data) do
    put_in(data_view.dirty, data.dirty)
  end
end

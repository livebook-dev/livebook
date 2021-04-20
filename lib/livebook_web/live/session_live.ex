defmodule LivebookWeb.SessionLive do
  use LivebookWeb, :live_view

  alias Livebook.{SessionSupervisor, Session, Delta, Notebook}

  @impl true
  def mount(%{"id" => session_id}, _session, socket) do
    if SessionSupervisor.session_exists?(session_id) do
      data =
        if connected?(socket) do
          data = Session.register_client(session_id, self())
          Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")

          data
        else
          Session.get_data(session_id)
        end

      platform = platform_from_socket(socket)

      {:ok,
       socket
       |> assign(platform: platform, session_id: session_id, data_view: data_to_view(data))
       |> assign_private(data: data)
       |> allow_upload(:cell_image,
         accept: ~w(.jpg .jpeg .png .gif),
         max_entries: 1,
         max_file_size: 5_000_000
       )}
    else
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
    ~L"""
    <div class="flex flex-grow h-full"
      id="session"
      data-element="session"
      phx-hook="Session">
      <div class="flex flex-col items-center space-y-5 px-3 py-7 bg-gray-900">
        <%= live_patch to: Routes.home_path(@socket, :page) do %>
          <img src="/logo.png" height="40" width="40" alt="livebook" />
        <% end %>
        <span class="tooltip right distant" aria-label="Sections (ss)">
          <button class="text-2xl text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center" data-element="sections-panel-toggle">
            <%= remix_icon("booklet-fill") %>
          </button>
        </span>
        <span class="tooltip right distant" aria-label="Notebook settings (sn)">
          <%= live_patch to: Routes.session_path(@socket, :settings, @session_id, "file"),
                class: "text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center #{if(@live_action == :settings, do: "text-gray-50 bg-gray-700")}" do %>
            <%= remix_icon("settings-4-fill", class: "text-2xl") %>
          <% end %>
        </span>
        <div class="flex-grow"></div>
        <span class="tooltip right distant" aria-label="Keyboard shortcuts (?)">
          <%= live_patch to: Routes.session_path(@socket, :shortcuts, @session_id),
                class: "text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center #{if(@live_action == :shortcuts, do: "text-gray-50 bg-gray-700")}" do %>
            <%= remix_icon("keyboard-box-fill", class: "text-2xl") %>
          <% end %>
        </span>
      </div>
      <div class="flex flex-col w-1/3 lg:w-1/5 overflow-y-auto bg-gray-50 border-r border-gray-100 px-6 py-10" data-element="sections-panel">
        <div class="flex-grow flex flex-col">
          <h3 class="font-semibold text-gray-800 text-lg">
            Sections
          </h3>
          <div class="mt-4 flex flex-col space-y-4" data-element="section-list">
            <%= for section_item <- @data_view.sections_items do %>
              <button class="text-left hover:text-gray-900 text-gray-500"
                data-element="section-list-item"
                data-section-id="<%= section_item.id %>">
                <%= section_item.name %>
              </button>
            <% end %>
          </div>
          <button class="mt-8 p-8 py-1 text-gray-500 text-sm font-medium rounded-xl border border-gray-400 border-dashed hover:bg-gray-100 inline-flex items-center justify-center space-x-2"
            phx-click="add_section" >
            <%= remix_icon("add-line", class: "text-lg align-center") %>
            <span>New section</span>
          </button>
        </div>
      </div>
      <div class="flex-grow overflow-y-auto" data-element="notebook">
        <div class="py-7 px-16 max-w-screen-lg w-full mx-auto">
          <div class="pb-4 mb-6 border-b border-gray-200">
            <h1 class="text-gray-800 font-semibold text-3xl p-1 -ml-1 rounded-lg border border-transparent hover:border-blue-200 focus:border-blue-300"
              id="notebook-name"
              data-element="notebook-name"
              contenteditable
              spellcheck="false"
              phx-blur="set_notebook_name"
              phx-hook="ContentEditable"
              data-update-attribute="phx-value-name"><%= @data_view.notebook_name %></h1>
          </div>
          <div class="flex flex-col w-full space-y-16">
            <%= if @data_view.section_views == [] do %>
              <div class="flex justify-center">
                <button class="button button-small"
                  phx-click="insert_section"
                  phx-value-index="0"
                  >+ Section</button>
              </div>
            <% end %>
            <%= for {section_view, index} <- Enum.with_index(@data_view.section_views) do %>
              <%= live_component @socket, LivebookWeb.SessionLive.SectionComponent,
                    id: section_view.id,
                    index: index,
                    session_id: @session_id,
                    section_view: section_view %>
            <% end %>
            <div style="height: 80vh"></div>
          </div>
        </div>
      </div>
      <div class="fixed bottom-[0.4rem] right-[1.5rem]">
        <%= live_component @socket, LivebookWeb.SessionLive.IndicatorsComponent,
              session_id: @session_id,
              data_view: @data_view %>
      </div>
    </div>

    <%= if @live_action == :settings do %>
      <%= live_modal @socket, LivebookWeb.SessionLive.SettingsComponent,
            id: :settings_modal,
            return_to: Routes.session_path(@socket, :page, @session_id),
            tab: @tab,
            session_id: @session_id,
            data_view: @data_view %>
    <% end %>

    <%= if @live_action == :shortcuts do %>
      <%= live_modal @socket, LivebookWeb.SessionLive.ShortcutsComponent,
            id: :shortcuts_modal,
            platform: @platform,
            return_to: Routes.session_path(@socket, :page, @session_id) %>
    <% end %>

    <%= if @live_action == :cell_settings do %>
      <%= live_modal @socket, LivebookWeb.SessionLive.CellSettingsComponent,
            id: :cell_settings_modal,
            session_id: @session_id,
            cell: @cell,
            return_to: Routes.session_path(@socket, :page, @session_id) %>
    <% end %>

    <%= if @live_action == :cell_upload do %>
      <%= live_modal @socket, LivebookWeb.SessionLive.CellUploadComponent,
            id: :cell_upload_modal,
            session_id: @session_id,
            cell: @cell,
            uploads: @uploads,
            return_to: Routes.session_path(@socket, :page, @session_id) %>
    <% end %>
    """
  end

  @impl true
  def handle_params(%{"cell_id" => cell_id}, _url, socket) do
    {:ok, cell, _} = Notebook.fetch_cell_and_section(socket.private.data.notebook, cell_id)
    {:noreply, assign(socket, cell: cell)}
  end

  def handle_params(%{"tab" => tab}, _url, socket) do
    {:noreply, assign(socket, tab: tab)}
  end

  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("cell_init", %{"cell_id" => cell_id}, socket) do
    data = socket.private.data

    case Notebook.fetch_cell_and_section(data.notebook, cell_id) do
      {:ok, cell, _section} ->
        payload = %{
          source: cell.source,
          revision: data.cell_infos[cell.id].revision
        }

        {:reply, payload, socket}

      :error ->
        {:noreply, socket}
    end
  end

  def handle_event("add_section", _params, socket) do
    end_index = length(socket.private.data.notebook.sections)
    Session.insert_section(socket.assigns.session_id, end_index)

    {:noreply, socket}
  end

  def handle_event("insert_section", %{"index" => index}, socket) do
    index = ensure_integer(index) |> max(0)
    Session.insert_section(socket.assigns.session_id, index)

    {:noreply, socket}
  end

  def handle_event("delete_section", %{"section_id" => section_id}, socket) do
    Session.delete_section(socket.assigns.session_id, section_id)

    {:noreply, socket}
  end

  def handle_event(
        "insert_cell",
        %{"section_id" => section_id, "index" => index, "type" => type},
        socket
      ) do
    index = ensure_integer(index) |> max(0)
    type = String.to_atom(type)
    Session.insert_cell(socket.assigns.session_id, section_id, index, type)

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
    Session.delete_cell(socket.assigns.session_id, cell_id)

    {:noreply, socket}
  end

  def handle_event("set_notebook_name", %{"name" => name}, socket) do
    name = normalize_name(name)
    Session.set_notebook_name(socket.assigns.session_id, name)

    {:noreply, socket}
  end

  def handle_event("set_section_name", %{"section_id" => section_id, "name" => name}, socket) do
    name = normalize_name(name)
    Session.set_section_name(socket.assigns.session_id, section_id, name)

    {:noreply, socket}
  end

  def handle_event(
        "apply_cell_delta",
        %{"cell_id" => cell_id, "delta" => delta, "revision" => revision},
        socket
      ) do
    delta = Delta.from_compressed(delta)
    Session.apply_cell_delta(socket.assigns.session_id, cell_id, delta, revision)

    {:noreply, socket}
  end

  def handle_event(
        "report_cell_revision",
        %{"cell_id" => cell_id, "revision" => revision},
        socket
      ) do
    Session.report_cell_revision(socket.assigns.session_id, cell_id, revision)

    {:noreply, socket}
  end

  def handle_event("move_cell", %{"cell_id" => cell_id, "offset" => offset}, socket) do
    offset = ensure_integer(offset)
    Session.move_cell(socket.assigns.session_id, cell_id, offset)

    {:noreply, socket}
  end

  def handle_event("move_section", %{"section_id" => section_id, "offset" => offset}, socket) do
    offset = ensure_integer(offset)
    Session.move_section(socket.assigns.session_id, section_id, offset)

    {:noreply, socket}
  end

  def handle_event("queue_cell_evaluation", %{"cell_id" => cell_id}, socket) do
    Session.queue_cell_evaluation(socket.assigns.session_id, cell_id)
    {:noreply, socket}
  end

  def handle_event("queue_section_cells_evaluation", %{"section_id" => section_id}, socket) do
    with {:ok, section} <- Notebook.fetch_section(socket.private.data.notebook, section_id) do
      for cell <- section.cells, cell.type == :elixir do
        Session.queue_cell_evaluation(socket.assigns.session_id, cell.id)
      end
    end

    {:noreply, socket}
  end

  def handle_event("queue_all_cells_evaluation", _params, socket) do
    data = socket.private.data

    for {cell, _} <- Notebook.elixir_cells_with_section(data.notebook),
        data.cell_infos[cell.id].validity_status != :evaluated do
      Session.queue_cell_evaluation(socket.assigns.session_id, cell.id)
    end

    {:noreply, socket}
  end

  def handle_event("queue_child_cells_evaluation", %{"cell_id" => cell_id}, socket) do
    with {:ok, cell, _section} <-
           Notebook.fetch_cell_and_section(socket.private.data.notebook, cell_id) do
      for {cell, _} <- Notebook.child_cells_with_section(socket.private.data.notebook, cell.id) do
        Session.queue_cell_evaluation(socket.assigns.session_id, cell.id)
      end
    end

    {:noreply, socket}
  end

  def handle_event("cancel_cell_evaluation", %{"cell_id" => cell_id}, socket) do
    Session.cancel_cell_evaluation(socket.assigns.session_id, cell_id)

    {:noreply, socket}
  end

  def handle_event("save", %{}, socket) do
    Session.save(socket.assigns.session_id)

    {:noreply, socket}
  end

  def handle_event("show_shortcuts", %{}, socket) do
    {:noreply,
     push_patch(socket, to: Routes.session_path(socket, :shortcuts, socket.assigns.session_id))}
  end

  def handle_event("show_notebook_settings", %{}, socket) do
    {:noreply,
     push_patch(socket,
       to: Routes.session_path(socket, :settings, socket.assigns.session_id, "file")
     )}
  end

  def handle_event("show_notebook_runtime_settings", %{}, socket) do
    {:noreply,
     push_patch(socket,
       to: Routes.session_path(socket, :settings, socket.assigns.session_id, "runtime")
     )}
  end

  @impl true
  def handle_info({:operation, operation}, socket) do
    case Session.Data.apply_operation(socket.private.data, operation) do
      {:ok, data, actions} ->
        new_socket =
          socket
          |> assign_private(data: data)
          |> assign(data_view: data_to_view(data))
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

  def handle_info(:session_closed, socket) do
    {:noreply,
     socket
     |> put_flash(:info, "Session has been closed")
     |> push_redirect(to: Routes.home_path(socket, :page))}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp after_operation(socket, _prev_socket, {:insert_section, client_pid, _index, section_id}) do
    if client_pid == self() do
      push_event(socket, "section_inserted", %{section_id: section_id})
    else
      socket
    end
  end

  defp after_operation(socket, _prev_socket, {:delete_section, _client_pid, section_id}) do
    push_event(socket, "section_deleted", %{section_id: section_id})
  end

  defp after_operation(socket, _prev_socket, {:insert_cell, client_pid, _, _, _, cell_id}) do
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
    Session.insert_cell(socket.assigns.session_id, section.id, index + idx_offset, type)
  end

  defp ensure_integer(n) when is_integer(n), do: n
  defp ensure_integer(n) when is_binary(n), do: String.to_integer(n)

  # Builds view-specific structure of data by cherry-picking
  # only the relevant attributes.
  # We then use `@data_view` in the templates and consequently
  # irrelevant changes to data don't change `@data_view`, so LV doesn't
  # have to traverse the whole template tree and no diff is sent to the client.
  defp data_to_view(data) do
    %{
      path: data.path,
      dirty: data.dirty,
      runtime: data.runtime,
      global_evaluation_status: global_evaluation_status(data),
      notebook_name: data.notebook.name,
      sections_items:
        for section <- data.notebook.sections do
          %{id: section.id, name: section.name}
        end,
      section_views: Enum.map(data.notebook.sections, &section_to_view(&1, data))
    }
  end

  defp global_evaluation_status(data) do
    cells =
      data.notebook
      |> Notebook.elixir_cells_with_section()
      |> Enum.map(fn {cell, _} -> cell end)

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

  defp evaluating?(cell, data), do: data.cell_infos[cell.id].evaluation_status == :evaluating

  defp stale?(cell, data), do: data.cell_infos[cell.id].validity_status == :stale

  defp evaluated?(cell, data), do: data.cell_infos[cell.id].validity_status == :evaluated

  defp section_to_view(section, data) do
    %{
      id: section.id,
      name: section.name,
      cell_views: Enum.map(section.cells, &cell_to_view(&1, data))
    }
  end

  defp cell_to_view(cell, data) do
    info = data.cell_infos[cell.id]

    %{
      id: cell.id,
      type: cell.type,
      empty?: cell.source == "",
      outputs: cell.outputs,
      validity_status: info.validity_status,
      evaluation_status: info.evaluation_status,
      changed?: info.evaluation_digest != nil and info.digest != info.evaluation_digest
    }
  end
end

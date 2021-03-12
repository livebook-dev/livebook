defmodule LivebookWeb.SessionLive do
  use LivebookWeb, :live_view

  alias Livebook.{SessionSupervisor, Session, Delta, Notebook, Runtime}

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

      {:ok, assign(socket, initial_assigns(session_id, data, platform))}
    else
      {:ok, redirect(socket, to: Routes.home_path(socket, :page))}
    end
  end

  defp platform_from_socket(socket) do
    with connect_info when connect_info != nil <- get_connect_info(socket),
         {:ok, user_agent} <- Map.fetch(connect_info, :user_agent) do
      platform_from_user_agent(user_agent)
    else
      _ -> nil
    end
  end

  defp initial_assigns(session_id, data, platform) do
    %{
      platform: platform,
      session_id: session_id,
      data: data
    }
  end

  @impl true
  def render(assigns) do
    ~L"""
    <%= if @live_action == :file do %>
      <%= live_modal @socket, LivebookWeb.SessionLive.PersistenceComponent,
            id: :file_modal,
            return_to: Routes.session_path(@socket, :page, @session_id),
            session_id: @session_id,
            path: @data.path %>
    <% end %>

    <%= if @live_action == :runtime do %>
      <%= live_modal @socket, LivebookWeb.SessionLive.RuntimeComponent,
            id: :runtime_modal,
            return_to: Routes.session_path(@socket, :page, @session_id),
            session_id: @session_id,
            runtime: @data.runtime %>
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

    <div class="flex flex-grow h-full"
      id="session"
      data-element="session"
      phx-hook="Session">
      <div class="flex flex-col w-1/5 bg-gray-100 border-r border-gray-200">
        <div class="flex-grow flex flex-col space-y-2 pl-4 pt-4"
          data-element="section-list">
          <%= for section <- @data.notebook.sections do %>
            <button class="py-2 px-4 rounded-l-md text-left hover:text-current text-gray-500"
              data-element="section-list-item"
              data-section-id="<%= section.id %>">
              <%= section.name %>
            </button>
          <% end %>
          <button phx-click="add_section" class="py-2 px-4 rounded-l-md cursor-pointer text-gray-300 hover:text-gray-400">
            <div class="flex items-center space-x-2">
              <%= remix_icon("add-line", class: "text-2xl") %>
              <span>New section</span>
            </div>
          </button>
        </div>
        <%= live_patch to: Routes.session_path(@socket, :runtime, @session_id) do %>
          <div class="text-sm text-gray-500 text-medium px-4 py-2 border-b border-gray-200 flex space-x-2 items-center hover:bg-gray-200">
            <%= remix_icon("cpu-line", class: "text-xl text-gray-400") %>
            <span><%= runtime_description(@data.runtime) %></span>
          </div>
        <% end %>
        <%= live_patch to: Routes.session_path(@socket, :file, @session_id) do %>
          <div class="text-sm text-gray-500 text-medium px-4 py-2 border-b border-gray-200 flex space-x-2 items-center hover:bg-gray-200">
            <%= if @data.path do %>
              <%= if @data.dirty do %>
                <%= remix_icon("refresh-line", class: "text-xl text-blue-600") %>
              <% else %>
                <%= remix_icon("checkbox-circle-line", class: "text-xl text-green-400") %>
              <% end %>
              <span>
                <%= Path.basename(@data.path) %>
              </span>
            <% else %>
              <%= remix_icon("file-code-line", class: "text-xl text-gray-400") %>
              <span>
                No file choosen
              </span>
            <% end %>
          </div>
        <% end %>
        <div class="p-4 flex space-x-2">
          <%= live_patch to: Routes.home_path(@socket, :page) do %>
            <%= remix_icon("home-2-line", class: "text-2xl text-gray-600 hover:text-current") %>
          <% end %>
          <%= live_patch to: Routes.session_path(@socket, :shortcuts, @session_id) do %>
            <%= remix_icon("question-line", class: "text-2xl text-gray-600 hover:text-current") %>
          <% end %>
        </div>
      </div>
      <div class="flex-grow px-6 py-8 flex overflow-y-auto" data-element="notebook">
        <div class="max-w-screen-lg w-full mx-auto">
          <div class="mb-8">
            <h1 class="text-gray-900 font-semibold text-4xl pb-2 border-b-2 border-transparent hover:border-blue-100 focus:border-blue-300"
              id="notebook-name"
              contenteditable
              spellcheck="false"
              phx-blur="set_notebook_name"
              phx-hook="ContentEditable"
              data-update-attribute="phx-value-name"><%= @data.notebook.name %></h1>
          </div>
          <div class="flex flex-col space-y-16">
            <%= for section <- @data.notebook.sections do %>
              <%= live_component @socket, LivebookWeb.SectionComponent,
                    id: section.id,
                    session_id: @session_id,
                    section: section,
                    cell_infos: @data.cell_infos %>
            <% end %>
            <div style="height: 80vh"></div>
          </div>
        </div>
      </div>
      <%# Show a tiny insert indicator for clarity %>
      <div class="fixed right-5 bottom-1 text-gray-500 text-semibold text-sm" data-element="insert-indicator">
        insert
      </div>
    </div>
    """
  end

  @impl true
  def handle_params(%{"cell_id" => cell_id}, _url, socket) do
    {:ok, cell, _} = Notebook.fetch_cell_and_section(socket.assigns.data.notebook, cell_id)
    {:noreply, assign(socket, cell: cell)}
  end

  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("cell_init", %{"cell_id" => cell_id}, socket) do
    data = socket.assigns.data

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
    end_index = length(socket.assigns.data.notebook.sections)
    Session.insert_section(socket.assigns.session_id, end_index)

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
    insert_cell_next_to(socket.assigns, cell_id, type, idx_offset: 1)

    {:noreply, socket}
  end

  def handle_event("insert_cell_above", %{"cell_id" => cell_id, "type" => type}, socket) do
    type = String.to_atom(type)
    insert_cell_next_to(socket.assigns, cell_id, type, idx_offset: 0)

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

  def handle_event("queue_cell_evaluation", %{"cell_id" => cell_id}, socket) do
    Session.queue_cell_evaluation(socket.assigns.session_id, cell_id)
    {:noreply, socket}
  end

  def handle_event("queue_section_cells_evaluation", %{"section_id" => section_id}, socket) do
    with {:ok, section} <- Notebook.fetch_section(socket.assigns.data.notebook, section_id) do
      for cell <- section.cells, cell.type == :elixir do
        Session.queue_cell_evaluation(socket.assigns.session_id, cell.id)
      end
    end

    {:noreply, socket}
  end

  def handle_event("queue_all_cells_evaluation", _params, socket) do
    data = socket.assigns.data

    for {cell, _} <- Notebook.elixir_cells_with_section(data.notebook),
        data.cell_infos[cell.id].validity_status != :evaluated do
      Session.queue_cell_evaluation(socket.assigns.session_id, cell.id)
    end

    {:noreply, socket}
  end

  def handle_event("queue_child_cells_evaluation", %{"cell_id" => cell_id}, socket) do
    with {:ok, cell, _section} <-
           Notebook.fetch_cell_and_section(socket.assigns.data.notebook, cell_id) do
      for {cell, _} <- Notebook.child_cells_with_section(socket.assigns.data.notebook, cell.id) do
        Session.queue_cell_evaluation(socket.assigns.session_id, cell.id)
      end
    end

    {:noreply, socket}
  end

  def handle_event("cancel_cell_evaluation", %{"cell_id" => cell_id}, socket) do
    Session.cancel_cell_evaluation(socket.assigns.session_id, cell_id)

    {:noreply, socket}
  end

  def handle_event("show_shortcuts", %{}, socket) do
    {:noreply,
     push_patch(socket, to: Routes.session_path(socket, :shortcuts, socket.assigns.session_id))}
  end

  @impl true
  def handle_info({:operation, operation}, socket) do
    case Session.Data.apply_operation(socket.assigns.data, operation) do
      {:ok, data, actions} ->
        new_socket =
          socket
          |> assign(data: data)
          |> after_operation(socket, operation)
          |> handle_actions(actions)

        {:noreply, new_socket}

      :error ->
        {:noreply, socket}
    end
  end

  def handle_info({:error, error}, socket) do
    message = error |> to_string() |> String.capitalize()

    {:noreply, put_flash(socket, :error, message)}
  end

  def handle_info({:info, info}, socket) do
    message = info |> to_string() |> String.capitalize()

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
      case Notebook.fetch_cell_sibling(prev_socket.assigns.data.notebook, cell_id, 1) do
        {:ok, next_cell} ->
          next_cell.id

        :error ->
          case Notebook.fetch_cell_sibling(prev_socket.assigns.data.notebook, cell_id, -1) do
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

  defp insert_cell_next_to(assigns, cell_id, type, idx_offset: idx_offset) do
    {:ok, cell, section} = Notebook.fetch_cell_and_section(assigns.data.notebook, cell_id)
    index = Enum.find_index(section.cells, &(&1 == cell))
    Session.insert_cell(assigns.session_id, section.id, index + idx_offset, type)
  end

  defp runtime_description(nil), do: "No runtime"
  defp runtime_description(%Runtime.ElixirStandalone{}), do: "Elixir standalone runtime"
  defp runtime_description(%Runtime.MixStandalone{}), do: "Mix standalone runtime"
  defp runtime_description(%Runtime.Attached{}), do: "Attached runtime"

  defp ensure_integer(n) when is_integer(n), do: n
  defp ensure_integer(n) when is_binary(n), do: String.to_integer(n)
end

defmodule LiveBookWeb.SessionLive do
  use LiveBookWeb, :live_view

  alias LiveBook.{SessionSupervisor, Session, Delta, Notebook}

  @impl true
  def mount(%{"id" => session_id}, _session, socket) do
    if SessionSupervisor.session_exists?(session_id) do
      data =
        if connected?(socket) do
          data = Session.register_client(session_id, self())
          Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

          data
        else
          Session.get_data(session_id)
        end

      {:ok, assign(socket, initial_assigns(session_id, data))}
    else
      {:ok, redirect(socket, to: Routes.live_path(socket, LiveBookWeb.SessionsLive))}
    end
  end

  defp initial_assigns(session_id, data) do
    first_section_id =
      case data.notebook.sections do
        [section | _] -> section.id
        [] -> nil
      end

    %{
      session_id: session_id,
      data: data,
      selected_section_id: first_section_id,
      focused_cell_id: nil,
      focused_cell_expanded: false
    }
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex flex-grow max-h-full"
         id="session"
         phx-hook="Session"
         data-focused-cell-id="<%= @focused_cell_id %>">
      <div class="w-1/5 bg-gray-100 border-r-2 border-gray-200">
        <h1 id="notebook-name"
            contenteditable
            spellcheck="false"
            phx-blur="set_notebook_name"
            phx-hook="ContentEditable"
            data-update-attribute="phx-value-name"
            class="p-8 text-2xl"><%= @data.notebook.name %></h1>
        <div class="flex flex-col space-y-2 pl-4">
          <%= for section <- @data.notebook.sections do %>
            <div phx-click="select_section"
                 phx-value-section_id="<%= section.id %>"
                 class="py-2 px-4 rounded-l-md cursor-pointer text-gray-500 hover:text-current">
              <span>
                <%= section.name %>
              </span>
            </div>
          <% end %>
          <button phx-click="add_section" class="py-2 px-4 rounded-l-md cursor-pointer text-gray-300 hover:text-gray-400">
            <div class="flex items-center space-x-2">
              <%= Icons.svg(:plus, class: "h-6") %>
              <span>New section</span>
            </div>
          </button>
        </div>
      </div>
      <div class="flex-grow px-6 py-8 flex overflow-y-auto">
        <div class="max-w-screen-lg w-full mx-auto">
          <%= for section <- @data.notebook.sections do %>
            <%= live_component @socket, LiveBookWeb.Section,
                               id: section.id,
                               section: section,
                               selected: section.id == @selected_section_id,
                               cell_infos: @data.cell_infos,
                               focused_cell_id: @focused_cell_id,
                               focused_cell_expanded: @focused_cell_expanded %>
          <% end %>
        </div>
      </div>
    </div>
    """
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

  def handle_event("select_section", %{"section_id" => section_id}, socket) do
    {:noreply, assign(socket, selected_section_id: section_id)}
  end

  def handle_event(
        "insert_cell",
        %{"section_id" => section_id, "index" => index, "type" => type},
        socket
      ) do
    index = String.to_integer(index) |> max(0)
    type = String.to_atom(type)
    Session.insert_cell(socket.assigns.session_id, section_id, index, type)

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
    Session.apply_cell_delta(socket.assigns.session_id, self(), cell_id, delta, revision)

    {:noreply, socket}
  end

  def handle_event("focus_cell", %{"cell_id" => cell_id}, socket) do
    {:noreply, assign(socket, focused_cell_id: cell_id, focused_cell_expanded: false)}
  end

  def handle_event("move_cell_focus", %{"offset" => offset}, socket) do
    case new_focused_cell_from_offset(socket.assigns, offset) do
      {:ok, cell} ->
        {:noreply, assign(socket, focused_cell_id: cell.id, focused_cell_expanded: false)}

      :error ->
        {:noreply, socket}
    end
  end

  def handle_event("toggle_cell_expanded", %{}, socket) do
    if socket.assigns.focused_cell_id do
      {:noreply, assign(socket, focused_cell_expanded: !socket.assigns.focused_cell_expanded)}
    else
      {:noreply, socket}
    end
  end

  def handle_event("queue_cell_evaluation", %{"cell_id" => cell_id}, socket) do
    Session.queue_cell_evaluation(socket.assigns.session_id, cell_id)

    {:noreply, socket}
  end

  def handle_event("queue_cell_evaluation", %{}, socket) do
    if socket.assigns.focused_cell_id do
      Session.queue_cell_evaluation(socket.assigns.session_id, socket.assigns.focused_cell_id)
    end

    {:noreply, socket}
  end

  @impl true
  def handle_info({:operation, operation}, socket) do
    case Session.Data.apply_operation(socket.assigns.data, operation) do
      {:ok, data, actions} ->
        new_socket = assign(socket, data: data)
        {:noreply, handle_actions(new_socket, actions)}

      :error ->
        {:noreply, socket}
    end
  end

  defp handle_actions(state, actions) do
    Enum.reduce(actions, state, &handle_action(&2, &1))
  end

  defp handle_action(socket, {:broadcast_delta, from, cell, delta}) do
    if from == self() do
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

  defp new_focused_cell_from_offset(assigns, offset) do
    cond do
      assigns.focused_cell_id ->
        # If a cell is focused, look up the appropriate sibling
        Notebook.fetch_cell_sibling(assigns.data.notebook, assigns.focused_cell_id, offset)

      assigns.selected_section_id ->
        # If no cell is focused, focus the first one for easier keyboard navigation.
        {:ok, section} =
          Notebook.fetch_section(assigns.data.notebook, assigns.selected_section_id)

        Enum.fetch(section.cells, 0)

      true ->
        :error
    end
  end
end

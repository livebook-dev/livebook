defmodule LiveBookWeb.SessionLive do
  use LiveBookWeb, :live_view

  alias LiveBook.{SessionSupervisor, Session}

  @impl true
  def mount(%{"id" => session_id}, _session, socket) do
    if SessionSupervisor.session_exists?(session_id) do
      if connected?(socket) do
        Session.register_client(session_id, self())
        Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")
      end

      data = Session.get_data(session_id)

      {:ok,
       assign(socket,
         session_id: session_id,
         data: data,
         selected_section_id: nil,
         focused_cell_id: nil
       )}
    else
      {:ok, redirect(socket, to: Routes.live_path(socket, LiveBookWeb.SessionsLive))}
    end
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex flex-grow max-h-full">
      <div class="w-1/5 bg-gray-100 border-r-2 border-gray-200">
        <h1 class="p-8 text-2xl" contenteditable spellcheck="false"><%= @data.notebook.name %></h1>
        <div class="flex flex-col space-y-2 pl-4">
          <%= for section <- @data.notebook.sections do %>
            <div phx-click="select_section" phx-value-section_id="<%= section.id %>" class="py-2 px-4 rounded-l-md cursor-pointer text-gray-500 hover:text-current">
              <span>
                <%= section.name %>
              </span>
            </div>
          <% end %>
          <div phx-click="add_section" class="py-2 px-4 rounded-l-md cursor-pointer text-gray-300 hover:text-gray-400">
            <div class="flex items-center space-x-2">
              <%= Icons.svg(:plus, class: "h-6") %>
              <span>New section</span>
            </div>
          </div>
        </div>
      </div>
      <div class="flex-grow px-6 py-8 flex overflow-y-auto">
        <div class="max-w-screen-lg w-full mx-auto">
          <%= for section <- @data.notebook.sections do %>
            <%= live_component @socket, LiveBookWeb.Section,
                               id: "section-#{section.id}",
                               section: section,
                               selected: section.id == @selected_section_id,
                               focused_cell_id: @focused_cell_id %>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  @impl true
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

  def handle_event("focus_cell", %{"cell_id" => cell_id}, socket) do
    {:noreply, assign(socket, focused_cell_id: cell_id)}
  end

  @impl true
  def handle_info({:operation, operation}, socket) do
    case Session.Data.apply_operation(socket.assigns.data, operation) do
      {:ok, data, _actions} ->
        {:noreply, assign(socket, data: data)}

      :error ->
        {:noreply, socket}
    end
  end
end

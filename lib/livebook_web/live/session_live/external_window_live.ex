defmodule LivebookWeb.SessionLive.ExternalWindowLive do
  use LivebookWeb, :live_view

  alias Livebook.{Notebook, Session, Sessions}
  alias Livebook.Notebook.Cell

  @impl true
  def mount(%{"id" => session_id, "type" => type} = params, _session, socket) do
    case Sessions.fetch_session(session_id) do
      {:ok, %{pid: session_pid}} ->
        {data, client_id} =
          if connected?(socket) do
            {data, client_id} =
              Session.register_client(session_pid, self(), socket.assigns.current_user)

            Session.subscribe(session_id)

            {data, client_id}
          else
            data = Session.get_data(session_pid)
            {data, nil}
          end

        session = Session.get_by_pid(session_pid)

        {:ok,
         socket
         |> assign(
           session: session,
           client_id: client_id,
           type: type,
           embedded?: params["embedded"] == "true",
           data_view: data_to_view(data)
         )
         |> assign_private(data: data)}

      :error ->
        # TODO: handle this error correctly
        {:ok, redirect(socket, to: ~p"/")}
    end
  end

  @impl true
  def mount(%{"id" => session_id}, _session, socket) do
    {:ok, redirect(socket, to: ~p"/sessions/#{session_id}")}
  end

  defp assign_private(socket, assigns) do
    Enum.reduce(assigns, socket, fn {key, value}, socket ->
      put_in(socket.private[key], value)
    end)
  end

  defp data_to_view(data) do
    changed_input_ids = Session.Data.changed_input_ids(data)

    %{
      notebook_name: data.notebook.name,
      output_view: update_in(data.notebook.output_panel, [
        Access.key(:rows),
        Access.all(),
        Access.key(:items),
        Access.all()
      ], fn item ->
        {:ok, cell, _section} = Notebook.fetch_cell_and_section(data.notebook, item.cell_id)
        Map.put(item, :outputs, cell.outputs)
      end)
    }
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id="external-window" phx-hook="ExternalWindow" data-window-embedded={@embedded?}>
      <div data-el-js-view-iframes phx-update="ignore" id="js-view-iframes"></div>
      <div class="flex items-center pb-4 mb-2 space-x-4 border-b border-gray-200 pr-20 md:pr-0">
        <h1 class="text-3xl font-semibold text-gray-800">
          <%= @data_view.notebook_name %>
        </h1>
      </div>
      <div class="grid grid-cols-12">
        <%= for output_row <- @data_view.output_view.rows do %>
          <div :for={item <- output_row.items} class="grid col-span-12">
            <LivebookWeb.Output.outputs
              outputs={item.outputs}
              dom_id_map={%{}}
              session_id={@session.id}
              session_pid={@session.pid}
              client_id={@client_id}
              cell_id={item.cell_id}
              input_views={%{}}
            />
          </div>
        <% end %>
      </div>
      <.external_window_menu :if={not @embedded?} />
    </div>
    """
  end

  defp external_window_menu(assigns) do
    ~H"""
    <div class="fixed top-[1rem] right-[1.5rem] z-[500]">
      <div class="tooltip left" data-tooltip="External Window Options" data-el-external-window-menu>
        <.menu id="external-window-menu" position={:bottom_right}>
          <:toggle>
            <button
              class="icon-button icon-outlined-button border-gray-200 hover:bg-gray-100 focus:bg-gray-100"
              aria-label="external window options"
            >
              <.remix_icon icon="more-2-fill" class="text-xl text-gray-400" />
            </button>
          </:toggle>
          <.menu_item>
            <button role="menuitem" data-el-external-window-popin-button>
              <.remix_icon icon="corner-left-down-fill" />
              <span>Pop-In</span>
            </button>
          </.menu_item>
          <.menu_item>
            <button role="menuitem" data-el-external-window-close-button>
              <.remix_icon icon="close-fill" />
              <span>Close</span>
            </button>
          </.menu_item>
        </.menu>
      </div>
    </div>
    """
  end

  @impl true
  def handle_info({:operation, operation}, socket) do
    socket =
      case Session.Data.apply_operation(socket.private.data, operation) do
        {:ok, data, _actions} ->
          socket
          |> assign_private(data: data)
          |> assign(data_to_view(data))

        :error ->
          socket
      end

    {:noreply, socket}
  end

  @impl true
  def handle_info(message, socket) do
    IO.inspect(message, label: "Not implemented for External Window")
    {:noreply, socket}
  end
end

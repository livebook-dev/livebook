defmodule LivebookWeb.SessionLive.ExternalWindowLive do
  use LivebookWeb, :live_view

  alias Livebook.{Session, Sessions}

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
        type = String.to_existing_atom(type)

        {:ok,
         socket
         |> assign(
           session: session,
           client_id: client_id,
           type: type,
           embedded?: params["embedded"] == "true",
           data_view: data_to_view(type, data)
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

  @impl true
  def render(%{type: :output_panel} = assigns) do
    ~H"""
    <div
      id="external-window"
      phx-hook="ExternalWindow"
      data-window-embedded={@embedded?}
      data-el-output-panel
    >
      <div
        data-el-js-view-iframes
        class="pointer-events-none"
        phx-update="ignore"
        id="js-view-iframes"
      >
      </div>
      <div class="flex items-center pb-4 mb-2 space-x-4 border-b border-gray-200 pr-20 md:pr-0">
        <h1 class="text-3xl font-semibold text-gray-800">
          <%= @data_view.notebook_name %>
        </h1>
      </div>
      <.live_component
        module={LivebookWeb.SessionLive.OutputPanelComponent}
        id="output-panel"
        session={@session}
        client_id={@client_id}
        output_views={@data_view.output_views}
      />
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
    {:noreply, handle_operation(socket, operation)}
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

  def handle_info(:session_closed, socket) do
    {:noreply, redirect_on_closed(socket)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp handle_operation(socket, operation) do
    case Session.Data.apply_operation(socket.private.data, operation) do
      {:ok, data, _actions} ->
        socket
        |> assign_private(data: data)
        |> assign(
          data_view:
            update_data_view(
              socket.assigns.type,
              socket.assigns.data_view,
              socket.private.data,
              data,
              operation
            )
        )
        |> after_operation(socket, operation)

      :error ->
        socket
    end
  end

  defp after_operation(socket, _prev_socket, {:move_output_to_new_row, client_id, cell_id, _row_index}) do
    if client_id == socket.assigns.client_id do
      push_event(socket, "output_panel_item_moved", %{cell_id: cell_id})
    else
      socket
    end
  end

  defp after_operation(socket, _prev_socket, {:move_output_to_new_location, client_id, cell_id, _row_index, _col_index}) do
    if client_id == socket.assigns.client_id do
      push_event(socket, "output_panel_item_moved", %{cell_id: cell_id})
    else
      socket
    end
  end

  defp after_operation(socket, _prev_socket, _operation), do: socket

  defp redirect_on_closed(socket) do
    socket
    |> put_flash(:info, "Session has been closed")
    |> push_navigate(to: ~p"/")
  end

  defp update_data_view(type, _data_view, _prev_data, data, operation) do
    case operation do
      # See LivebookWeb.SessionLive for more details
      _ ->
        data_to_view(type, data)
    end
  end

  defp data_to_view(:output_panel, data) do
    %{
      notebook_name: data.notebook.name,
      output_views: enrich_output_panel_data(data)
    }
  end
end

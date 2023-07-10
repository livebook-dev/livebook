defmodule LivebookWeb.AppSessionLive do
  use LivebookWeb, :live_view

  import LivebookWeb.AppHelpers

  alias Livebook.Session
  alias Livebook.Notebook
  alias Livebook.Notebook.Cell

  @impl true
  def mount(%{"slug" => slug, "id" => session_id}, _session, socket)
      when socket.assigns.app_authenticated? do
    {:ok, app} = Livebook.Apps.fetch_app(slug)

    app_session = Enum.find(app.sessions, &(&1.id == session_id))

    if app_session && app_session.app_status.lifecycle == :active do
      %{pid: session_pid} = app_session
      session = Session.get_by_pid(session_pid)

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

      data_view = data_to_view(data)
      IO.inspect(data_view.output_layout)

      {:ok,
       socket
       |> assign(
         slug: slug,
         session: session,
         page_title: get_page_title(data.notebook.name),
         client_id: client_id,
         data_view: data_view
       )
       |> assign_private(data: data)
       |> push_event("load_layout", %{layout: data_view.output_layout})
      }
    else
      {:ok,
       assign(socket,
         nonexistent?: true,
         slug: slug,
         page_title: get_page_title(app.notebook_name)
       )}
    end
  end

  def mount(%{"slug" => slug} = params, _session, socket) do
    if connected?(socket) do
      to =
        if id = params["id"] do
          ~p"/apps/#{slug}/authenticate?id=#{id}"
        else
          ~p"/apps/#{slug}/authenticate"
        end

      {:ok, push_navigate(socket, to: to)}
    else
      {:ok, socket}
    end
  end

  # Puts the given assigns in `socket.private`,
  # to ensure they are not used for rendering.
  defp assign_private(socket, assigns) do
    Enum.reduce(assigns, socket, fn {key, value}, socket ->
      put_in(socket.private[key], value)
    end)
  end

  @impl true
  def render(%{nonexistent?: true} = assigns) when assigns.app_authenticated? do
    ~H"""
    <div class="h-screen flex items-center justify-center">
      <div class="flex flex-col space-y-4 items-center">
        <a href={~p"/"}>
          <img src={~p"/images/logo.png"} height="128" width="128" alt="livebook" />
        </a>
        <div class="text-2xl text-gray-800">
          This app session does not exist
        </div>
        <div class="max-w-2xl text-center text-gray-700">
          <span>Visit the</span>
          <.link class="border-b border-gray-700 hover:border-none" navigate={~p"/apps/#{@slug}"}>app page</.link>.
        </div>
      </div>
    </div>
    """
  end

  def render(%{data_view: %{output_type: :dashboard}} = assigns) when assigns.app_authenticated? do
    ~H"""
    <div class="h-full w-full" data-el-notebook>
      <div class="h-full w-full" data-el-notebook-content>
        <div id="app_dashboard" class="grid-stack" phx-hook="GridstackStatic" />
        <div data-el-js-view-iframes phx-update="ignore" id="js-view-iframes"></div>
        <div :for={output_view <- Enum.reverse(@data_view.output_views)} id={output_view.cell_id}>
          <LivebookWeb.Output.outputs
            outputs={[output_view.output]}
            dom_id_map={%{}}
            session_id={@session.id}
            session_pid={@session.pid}
            client_id={@client_id}
            cell_id={output_view.cell_id}
            input_values={output_view.input_values}
          />
        </div>
      </div>
    </div>
    """
  end

  def render(assigns) when assigns.app_authenticated? do
    ~H"""
    <div class="h-full relative overflow-y-auto px-4 md:px-20" data-el-notebook>
      <div class="w-full max-w-screen-lg py-4 mx-auto" data-el-notebook-content>
        <div class="absolute md:fixed right-8 md:left-4 top-3 w-10 h-10">
          <.menu id="app-menu" position={:bottom_left}>
            <:toggle>
              <button class="flex items-center text-gray-900">
                <img src={~p"/images/logo.png"} height="40" width="40" alt="logo livebook" />
                <.remix_icon icon="arrow-down-s-line" />
              </button>
            </:toggle>
            <.menu_item>
              <.link navigate={~p"/"} role="menuitem">
                <.remix_icon icon="home-6-line" />
                <span>Home</span>
              </.link>
            </.menu_item>
            <.menu_item :if={@data_view.multi_session}>
              <.link navigate={~p"/apps/#{@data_view.slug}"} role="menuitem">
                <.remix_icon icon="play-list-add-line" />
                <span>Sessions</span>
              </.link>
            </.menu_item>
            <.menu_item :if={@data_view.show_source}>
              <.link patch={~p"/apps/#{@data_view.slug}/#{@session.id}/source"} role="menuitem">
                <.remix_icon icon="code-line" />
                <span>View source</span>
              </.link>
            </.menu_item>
            <.menu_item :if={@livebook_authenticated?}>
              <.link patch={~p"/sessions/#{@session.id}"} role="menuitem">
                <.remix_icon icon="terminal-line" />
                <span>Debug</span>
              </.link>
            </.menu_item>
          </.menu>
        </div>
        <div data-el-js-view-iframes phx-update="ignore" id="js-view-iframes"></div>
        <div class="flex items-center pb-4 mb-2 space-x-4 border-b border-gray-200 pr-20 md:pr-0">
          <h1 class="text-3xl font-semibold text-gray-800">
            <%= @data_view.notebook_name %>
          </h1>
        </div>
        <div class="pt-4 flex flex-col space-y-6" data-el-outputs-container id="outputs">
          <%= if @data_view.app_status.execution == :error do %>
            <div class="error-box flex items-center gap-2">
              <.remix_icon icon="error-warning-line" class="text-xl" />
              <span>Something went wrong</span>
            </div>
          <% else %>
            <div :for={output_view <- Enum.reverse(@data_view.output_views)}>
              <LivebookWeb.Output.outputs
                outputs={[output_view.output]}
                dom_id_map={%{}}
                session_id={@session.id}
                session_pid={@session.pid}
                client_id={@client_id}
                cell_id={output_view.cell_id}
                input_values={output_view.input_values}
              />
            </div>
          <% end %>
        </div>
        <div style="height: 80vh"></div>
      </div>
      <div :if={show_app_status?(@data_view.app_status)} class="fixed right-6 bottom-4">
        <.app_status status={@data_view.app_status} />
      </div>
    </div>

    <.modal
      :if={@live_action == :source and @data_view.show_source}
      id="source-modal"
      show
      width={:big}
      patch={~p"/apps/#{@data_view.slug}/#{@session.id}"}
    >
      <.live_component
        module={LivebookWeb.AppSessionLive.SourceComponent}
        id="source"
        session={@session}
      />
    </.modal>
    """
  end

  def render(assigns), do: auth_placeholder(assigns)

  defp get_page_title(notebook_name) do
    "Livebook - #{notebook_name}"
  end

  @impl true
  def handle_params(_params, _url, socket), do: {:noreply, socket}

  @impl true
  def handle_event("queue_interrupted_cell_evaluation", %{"cell_id" => cell_id}, socket) do
    data = socket.private.data

    with {:ok, cell, _section} <- Notebook.fetch_cell_and_section(data.notebook, cell_id),
         true <- data.cell_infos[cell.id].eval.interrupted do
      Session.queue_full_evaluation(socket.assigns.session.pid, [cell_id])
    end

    {:noreply, socket}
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
            update_data_view(socket.assigns.data_view, socket.private.data, data, operation)
        )
        |> after_operation(socket, operation)

      :error ->
        socket
    end
  end

  defp after_operation(socket, _prev_socket, {:app_deactivate, _client_id}) do
    redirect_on_closed(socket)
  end

  defp after_operation(socket, _prev_socket, {:app_shutdown, _client_id}) do
    put_flash(
      socket,
      :info,
      "A new version has been deployed, this session will close once everybody leaves"
    )
  end

  defp after_operation(socket, _prev_socket, _operation), do: socket

  defp redirect_on_closed(socket) do
    socket
    |> put_flash(:info, "Session has been closed")
    |> push_navigate(to: ~p"/")
  end

  defp update_data_view(data_view, _prev_data, data, operation) do
    case operation do
      # See LivebookWeb.SessionLive for more details
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

      _ ->
        data_to_view(data)
    end
  end

  defp data_to_view(data) do
    IO.inspect(data, label: "DATA")
    %{
      notebook_name: data.notebook.name,
      output_views:
        for(
          {cell_id, output} <- visible_outputs(data.notebook),
          do: %{
            output: output,
            input_values: input_values_for_output(output, data),
            cell_id: cell_id
          }
        ),
      output_layout: data.notebook.app_settings.output_layout,
      output_type: data.notebook.app_settings.output_type,
      app_status: data.app_data.status,
      show_source: data.notebook.app_settings.show_source,
      slug: data.notebook.app_settings.slug,
      multi_session: data.notebook.app_settings.multi_session
    }
  end

  defp input_values_for_output(output, data) do
    input_ids = for attrs <- Cell.find_inputs_in_output(output), do: attrs.id
    Map.take(data.input_values, input_ids)
  end

  defp visible_outputs(notebook) do
    for section <- Enum.reverse(notebook.sections),
        cell <- Enum.reverse(section.cells),
        Cell.evaluable?(cell),
        output <- filter_outputs(cell.outputs, notebook.app_settings.output_type),
        do: {cell.id, output}
  end

  defp show_app_status?(%{execution: :executed, lifecycle: :active}), do: false
  defp show_app_status?(_status), do: true
end

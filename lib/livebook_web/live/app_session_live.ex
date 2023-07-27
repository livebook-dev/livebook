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

      {:ok,
       socket
       |> assign(
         slug: slug,
         session: session,
         page_title: get_page_title(data.notebook.name),
         client_id: client_id,
         data_view: data_to_view(data)
       )
       |> assign_private(data: data)}
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

  def render(assigns) when assigns.app_authenticated? do
    ~H"""
    <div class="h-full relative overflow-y-auto px-4 md:px-20" data-el-notebook>
      <div class="w-full max-w-screen-lg py-4 mx-auto" data-el-notebook-content>
        <div class="absolute md:fixed right-4 md:left-4 md:right-auto top-3">
          <.menu id="app-menu" position={:bottom_right} md_position={:bottom_left}>
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
          <div :for={output_view <- Enum.reverse(@data_view.output_views)}>
            <LivebookWeb.Output.outputs
              outputs={[output_view.output]}
              dom_id_map={%{}}
              session_id={@session.id}
              session_pid={@session.pid}
              client_id={@client_id}
              cell_id={output_view.cell_id}
              input_views={output_view.input_views}
            />
          </div>
          <%= if @data_view.app_status.execution == :error do %>
            <div class={[
              "flex justify-between items-center px-4 py-2 border-l-4 shadow-custom-1",
              "text-red-400 border-red-400"
            ]}>
              <div>
                Something went wrong
              </div>
              <div class="flex items-center gap-6">
                <span class="tooltip top" data-tooltip="Debug">
                  <.link
                    :if={@livebook_authenticated?}
                    navigate={~p"/sessions/#{@session.id}" <> "#cell-#{@data_view.errored_cell_id}"}
                  >
                    <.remix_icon icon="terminal-line" />
                  </.link>
                </span>
                <button
                  class={[
                    "button-base bg-transparent",
                    "border-red-400 text-red-400 hover:bg-red-50 focus:bg-red-50"
                  ]}
                  phx-click="queue_errored_cells_evaluation"
                >
                  <.remix_icon icon="play-circle-fill" class="align-middle mr-1" />
                  <span>Retry</span>
                </button>
              </div>
            </div>
          <% end %>
        </div>
        <div style="height: 80vh"></div>
      </div>
      <div class="fixed right-3 bottom-4 flex flex-col gap-2 items-center text-gray-600 w-10">
        <span
          :if={
            @data_view.app_status.execution == :executed and
              @data_view.any_stale?
          }
          class="tooltip left"
          data-tooltip={
            ~S'''
            Some inputs have changed.
            Click this button to process with latest values.
            '''
          }
        >
          <button phx-click="queue_full_evaluation" class="icon-button">
            <.remix_icon icon="play-circle-fill" class="text-3xl" />
          </button>
        </span>
        <.app_status_circle status={@data_view.app_status} />
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

  attr :status, :map, required: true

  defp app_status_circle(%{status: %{lifecycle: :shutting_down}} = assigns) do
    ~H"""
    <.app_status_indicator text="Shutting down" variant={:inactive} icon="stop-line" />
    """
  end

  defp app_status_circle(%{status: %{lifecycle: :deactivated}} = assigns) do
    ~H"""
    <.app_status_indicator text="Deactivated" variant={:inactive} icon="stop-line" />
    """
  end

  defp app_status_circle(%{status: %{execution: :executing}} = assigns) do
    ~H"""
    <.app_status_indicator text="Executing" variant={:progressing} icon="loader-3-line" spinning />
    """
  end

  defp app_status_circle(%{status: %{execution: :executed}} = assigns) do
    ~H"""
    <.app_status_indicator text="Executed" variant={:success} icon="check-line" />
    """
  end

  defp app_status_circle(%{status: %{execution: :error}} = assigns) do
    ~H"""
    <.app_status_indicator text="Error" variant={:error} icon="close-line" />
    """
  end

  defp app_status_circle(%{status: %{execution: :interrupted}} = assigns) do
    ~H"""
    <.app_status_indicator text="Interrupted" variant={:waiting} icon="pause-line" />
    """
  end

  attr :text, :string, required: true
  attr :variant, :atom, required: true
  attr :icon, :string, required: true
  attr :spinning, :boolean, default: false

  defp app_status_indicator(assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip={@text}>
      <span class={[
        status_circle_class(@variant),
        "opacity-75",
        "w-6 h-6 rounded-full flex items-center justify-center",
        @spinning && "animate-spin"
      ]}>
        <.remix_icon icon={@icon} class="text-white font-bold" />
      </span>
    </span>
    """
  end

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

  def handle_event("queue_errored_cells_evaluation", %{}, socket) do
    data = socket.private.data

    errored_cell_ids =
      for {cell_id, %{eval: eval_info}} <- data.cell_infos, eval_info.errored, do: cell_id

    Session.queue_full_evaluation(socket.assigns.session.pid, errored_cell_ids)

    {:noreply, socket}
  end

  def handle_event("queue_full_evaluation", %{}, socket) do
    Session.queue_full_evaluation(socket.assigns.session.pid, [])
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
    changed_input_ids = Session.Data.changed_input_ids(data)

    %{
      notebook_name: data.notebook.name,
      output_views:
        for(
          {cell_id, output} <- visible_outputs(data.notebook),
          do: %{
            output: output,
            input_views: input_views_for_output(output, data, changed_input_ids),
            cell_id: cell_id
          }
        ),
      app_status: data.app_data.status,
      show_source: data.notebook.app_settings.show_source,
      slug: data.notebook.app_settings.slug,
      multi_session: data.notebook.app_settings.multi_session,
      errored_cell_id: errored_cell_id(data),
      any_stale?: any_stale?(data)
    }
  end

  defp errored_cell_id(data) do
    data.notebook
    |> Notebook.evaluable_cells_with_section()
    |> Enum.find_value(fn {cell, _section} ->
      data.cell_infos[cell.id].eval.errored && cell.id
    end)
  end

  defp any_stale?(data) do
    Enum.any?(data.cell_infos, &match?({_, %{eval: %{validity: :stale}}}, &1))
  end

  defp input_views_for_output(output, data, changed_input_ids) do
    input_ids = for attrs <- Cell.find_inputs_in_output(output), do: attrs.id

    data.input_infos
    |> Map.take(input_ids)
    |> Map.new(fn {input_id, %{value: value}} ->
      {input_id, %{value: value, changed: MapSet.member?(changed_input_ids, input_id)}}
    end)
  end

  defp visible_outputs(notebook) do
    for section <- Enum.reverse(notebook.sections),
        cell <- Enum.reverse(section.cells),
        Cell.evaluable?(cell),
        output <- filter_outputs(cell.outputs, notebook.app_settings.output_type),
        do: {cell.id, output}
  end

  defp filter_outputs(outputs, :all), do: outputs
  defp filter_outputs(outputs, :rich), do: rich_outputs(outputs)

  defp rich_outputs(outputs) do
    for output <- outputs, output = filter_output(output), do: output
  end

  defp filter_output({idx, output})
       when elem(output, 0) in [:plain_text, :markdown, :image, :js, :control, :input],
       do: {idx, output}

  defp filter_output({idx, {:tabs, outputs, metadata}}) do
    outputs_with_labels =
      for {output, label} <- Enum.zip(outputs, metadata.labels),
          output = filter_output(output),
          do: {output, label}

    {outputs, labels} = Enum.unzip(outputs_with_labels)

    {idx, {:tabs, outputs, %{metadata | labels: labels}}}
  end

  defp filter_output({idx, {:grid, outputs, metadata}}) do
    outputs = rich_outputs(outputs)

    if outputs != [] do
      {idx, {:grid, outputs, metadata}}
    end
  end

  defp filter_output({idx, {:frame, outputs, metadata}}) do
    outputs = rich_outputs(outputs)
    {idx, {:frame, outputs, metadata}}
  end

  defp filter_output({idx, {:error, _message, {:interrupt, _, _}} = output}),
    do: {idx, output}

  defp filter_output(_output), do: nil
end

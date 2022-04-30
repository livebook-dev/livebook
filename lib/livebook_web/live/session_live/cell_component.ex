defmodule LivebookWeb.SessionLive.CellComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col relative"
      data-el-cell
      id={"cell-#{@cell_view.id}"}
      phx-hook="Cell"
      data-cell-id={@cell_view.id}
      data-focusable-id={@cell_view.id}
      data-type={@cell_view.type}
      data-session-path={Routes.session_path(@socket, :page, @session_id)}
      data-evaluation-digest={get_in(@cell_view, [:eval, :evaluation_digest])}
      data-eval-validity={get_in(@cell_view, [:eval, :validity])}
      data-js-empty={empty?(@cell_view.source_view)}>
      <%= render_cell(assigns) %>
    </div>
    """
  end

  defp render_cell(%{cell_view: %{type: :markdown}} = assigns) do
    ~H"""
    <.cell_actions>
      <:secondary>
        <.enable_insert_mode_button />
        <.insert_image_button cell_id={@cell_view.id} session_id={@session_id} socket={@socket} />
        <.cell_link_button cell_id={@cell_view.id} />
        <.move_cell_up_button cell_id={@cell_view.id} />
        <.move_cell_down_button cell_id={@cell_view.id} />
        <.delete_cell_button cell_id={@cell_view.id} />
      </:secondary>
    </.cell_actions>
    <.cell_body>
      <div class="pb-4" data-el-editor-box>
        <.live_component module={LivebookWeb.SessionLive.CellEditorComponent}
          id={"#{@cell_view.id}-primary"}
          cell_id={@cell_view.id}
          tag="primary"
          source_view={@cell_view.source_view}
          language="markdown" />
      </div>
      <div class="markdown"
        data-el-markdown-container
        id={"markdown-container-#{@cell_view.id}"}
        phx-update="ignore">
        <.content_skeleton empty={empty?(@cell_view.source_view)} />
      </div>
    </.cell_body>
    """
  end

  defp render_cell(%{cell_view: %{type: :code}} = assigns) do
    ~H"""
    <.cell_actions>
      <:primary>
        <.cell_evaluation_button
          session_id={@session_id}
          socket={@socket}
          cell_id={@cell_view.id}
          validity={@cell_view.eval.validity}
          status={@cell_view.eval.status}
          reevaluate_automatically={@cell_view.reevaluate_automatically} />
      </:primary>
      <:secondary>
        <.amplify_output_button />
        <.cell_settings_button cell_id={@cell_view.id} socket={@socket} session_id={@session_id} />
        <.cell_link_button cell_id={@cell_view.id} />
        <.move_cell_up_button cell_id={@cell_view.id} />
        <.move_cell_down_button cell_id={@cell_view.id} />
        <.delete_cell_button cell_id={@cell_view.id} />
      </:secondary>
    </.cell_actions>
    <.cell_body>
      <div class="relative">
        <.live_component module={LivebookWeb.SessionLive.CellEditorComponent}
          id={"#{@cell_view.id}-primary"}
          cell_id={@cell_view.id}
          tag="primary"
          source_view={@cell_view.source_view}
          language="elixir"
          intellisense />
        <div class="absolute bottom-2 right-2">
          <.cell_status id={@cell_view.id} cell_view={@cell_view} />
        </div>
      </div>
      <.evaluation_outputs
        cell_view={@cell_view}
        socket={@socket}
        session_id={@session_id} />
    </.cell_body>
    """
  end

  defp render_cell(%{cell_view: %{type: :setup}} = assigns) do
    ~H"""
    <.cell_actions>
      <:primary>
        <.setup_cell_evaluation_button
          cell_id={@cell_view.id}
          validity={@cell_view.eval.validity}
          status={@cell_view.eval.status} />
      </:primary>
      <:secondary>
        <.enable_insert_mode_button />
        <.package_search_button session_id={@session_id} runtime={@runtime} socket={@socket} />
        <.cell_link_button cell_id={@cell_view.id} />
        <.setup_cell_info />
      </:secondary>
    </.cell_actions>
    <.cell_body>
      <div data-el-info-box>
        <div class="p-3 flex items-center justify-between border border-gray-200 text-sm text-gray-400 font-medium rounded-lg">
          <span>Notebook dependencies and setup</span>
          <.cell_status id={"#{@cell_view.id}-1"} cell_view={@cell_view} />
        </div>
      </div>
      <div data-el-editor-box>
        <div class="relative">
          <.live_component module={LivebookWeb.SessionLive.CellEditorComponent}
            id={"#{@cell_view.id}-primary"}
            cell_id={@cell_view.id}
            tag="primary"
            source_view={@cell_view.source_view}
            language="elixir"
            intellisense />
          <div class="absolute bottom-2 right-2">
            <.cell_status id={"#{@cell_view.id}-2"} cell_view={@cell_view} />
          </div>
        </div>
        <.evaluation_outputs
          cell_view={@cell_view}
          socket={@socket}
          session_id={@session_id} />
      </div>
    </.cell_body>
    """
  end

  defp render_cell(%{cell_view: %{type: :smart}} = assigns) do
    ~H"""
    <.cell_actions>
      <:primary>
        <.cell_evaluation_button
          session_id={@session_id}
          socket={@socket}
          cell_id={@cell_view.id}
          validity={@cell_view.eval.validity}
          status={@cell_view.eval.status}
          reevaluate_automatically={false} />
      </:primary>
      <:secondary>
        <.toggle_source_button />
        <.convert_smart_cell_button cell_id={@cell_view.id} />
        <.cell_link_button cell_id={@cell_view.id} />
        <.move_cell_up_button cell_id={@cell_view.id} />
        <.move_cell_down_button cell_id={@cell_view.id} />
        <.delete_cell_button cell_id={@cell_view.id} />
      </:secondary>
    </.cell_actions>
    <.cell_body>
      <div data-el-ui-box>
        <%= case @cell_view.status do %>
          <% :started -> %>
            <div class={"flex #{if(@cell_view.editor && @cell_view.editor.placement == :top, do: "flex-col-reverse", else: "flex-col")}"}>
              <.live_component module={LivebookWeb.JSViewComponent}
                id={@cell_view.id}
                js_view={@cell_view.js_view}
                session_id={@session_id} />
              <%= if @cell_view.editor do %>
                <.live_component module={LivebookWeb.SessionLive.CellEditorComponent}
                  id={"#{@cell_view.id}-secondary"}
                  cell_id={@cell_view.id}
                  tag="secondary"
                  source_view={@cell_view.editor.source_view}
                  language={@cell_view.editor.language}
                  rounded={@cell_view.editor.placement} />
              <% end %>
            </div>

          <% :dead -> %>
            <div class="info-box">
              <%= if @installing? do %>
                Waiting for dependency installation to complete...
              <% else %>
                Run the notebook setup to show the contents of this Smart cell.
              <% end %>
            </div>

          <% :starting -> %>
            <div class="delay-200">
              <.content_skeleton empty={false} />
            </div>
        <% end %>
        <div class="flex flex-col items-end space-y-2">
          <div></div>
          <.cell_status id={"#{@cell_view.id}-1"} cell_view={@cell_view} />
        </div>
      </div>
      <div data-el-editor-box>
        <div class="relative">
          <.live_component module={LivebookWeb.SessionLive.CellEditorComponent}
            id={"#{@cell_view.id}-primary"}
            cell_id={@cell_view.id}
            tag="primary"
            source_view={@cell_view.source_view}
            language="elixir"
            intellisense
            read_only />
          <div class="absolute bottom-2 right-2">
            <.cell_status id={"#{@cell_view.id}-2"} cell_view={@cell_view} />
          </div>
        </div>
      </div>
      <.evaluation_outputs
        cell_view={@cell_view}
        socket={@socket}
        session_id={@session_id} />
    </.cell_body>
    """
  end

  defp cell_actions(assigns) do
    assigns =
      assigns
      |> assign_new(:primary, fn -> [] end)
      |> assign_new(:secondary, fn -> [] end)

    ~H"""
    <div class="mb-1 flex items-center justify-between">
      <div class="relative z-20 flex items-center justify-end space-x-2" data-el-actions data-primary>
        <%= render_slot(@primary) %>
      </div>
      <div class="relative z-20 flex items-center justify-end space-x-2"
        role="toolbar"
        aria-label="cell actions"
        data-el-actions>
        <%= render_slot(@secondary) %>
      </div>
    </div>
    """
  end

  defp cell_body(assigns) do
    ~H"""
    <!-- By setting tabindex we can programmatically focus this element,
         also we actually want to make this element tab-focusable -->
    <div class="flex relative" data-el-cell-body tabindex="0">
      <div class="w-1 h-full rounded-lg absolute top-0 -left-3" data-el-cell-focus-indicator>
      </div>
      <div class="w-full">
        <%= render_slot(@inner_block) %>
      </div>
    </div>
    """
  end

  defp cell_evaluation_button(%{status: :ready, reevaluate_automatically: true} = assigns)
       when assigns.validity in [:evaluated, :stale] do
    ~H"""
    <%= live_patch to: Routes.session_path(@socket, :cell_settings, @session_id, @cell_id),
          class: "text-gray-600 hover:text-gray-800 focus:text-gray-800 flex space-x-1 items-center" do %>
      <.remix_icon icon="check-line" class="text-xl" />
      <span class="text-sm font-medium">
        Reevaluates automatically
      </span>
    <% end %>
    """
  end

  defp cell_evaluation_button(%{status: :ready} = assigns) do
    ~H"""
    <button class="text-gray-600 hover:text-gray-800 focus:text-gray-800 flex space-x-1 items-center"
      phx-click="queue_cell_evaluation"
      phx-value-cell_id={@cell_id}>
      <.remix_icon icon="play-circle-fill" class="text-xl" />
      <span class="text-sm font-medium">
        <%= if(@validity == :evaluated, do: "Reevaluate", else: "Evaluate") %>
      </span>
    </button>
    """
  end

  defp cell_evaluation_button(assigns) do
    ~H"""
    <button class="text-gray-600 hover:text-gray-800 focus:text-gray-800 flex space-x-1 items-center"
      phx-click="cancel_cell_evaluation"
      phx-value-cell_id={@cell_id}>
      <.remix_icon icon="stop-circle-fill" class="text-xl" />
      <span class="text-sm font-medium">
        Stop
      </span>
    </button>
    """
  end

  defp setup_cell_evaluation_button(%{status: :ready} = assigns) do
    ~H"""
    <button class="text-gray-600 hover:text-gray-800 focus:text-gray-800 flex space-x-1 items-center"
      phx-click="queue_cell_evaluation"
      phx-value-cell_id={@cell_id}>
      <%= if @validity == :fresh do %>
        <.remix_icon icon="play-circle-fill" class="text-xl" />
        <span class="text-sm font-medium">Setup</span>
      <% else %>
        <.remix_icon icon="restart-fill" class="text-xl" />
        <span class="text-sm font-medium">Reconnect and setup</span>
      <% end %>
    </button>
    """
  end

  defp setup_cell_evaluation_button(assigns) do
    ~H"""
    <button class="text-gray-600 hover:text-gray-800 focus:text-gray-800 flex space-x-1 items-center"
      phx-click="cancel_cell_evaluation"
      phx-value-cell_id={@cell_id}>
      <.remix_icon icon="stop-circle-fill" class="text-xl" />
      <span class="text-sm font-medium">
        Stop
      </span>
    </button>
    """
  end

  defp enable_insert_mode_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Edit content" data-el-enable-insert-mode-button>
      <button class="icon-button" aria-label="edit content">
        <.remix_icon icon="pencil-line" class="text-xl" />
      </button>
    </span>
    """
  end

  defp insert_image_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Insert image" data-el-insert-image-button>
      <%= live_patch to: Routes.session_path(@socket, :cell_upload, @session_id, @cell_id),
            class: "icon-button",
            aria_label: "insert image",
            role: "button" do %>
        <.remix_icon icon="image-add-line" class="text-xl" />
      <% end %>
    </span>
    """
  end

  defp toggle_source_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Toggle source" data-el-toggle-source-button>
      <button class="icon-button" aria-label="toggle source">
        <.remix_icon icon="code-line" class="text-xl" />
      </button>
    </span>
    """
  end

  defp convert_smart_cell_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Convert to Code cell">
      <button class="icon-button"
        aria-label="toggle source"
        data-link-package-search
        phx-click={
          with_confirm(
            JS.push("convert_smart_cell", value: %{cell_id: @cell_id}),
            title: "Convert cell",
            description: "Once you convert this Smart cell to a Code cell, the Smart cell will be moved to the bin.",
            confirm_text: "Convert",
            confirm_icon: "arrow-up-down-line",
            opt_out_id: "convert-smart-cell"
          )
        }>
        <.remix_icon icon="pencil-line" class="text-xl" />
      </button>
    </span>
    """
  end

  defp package_search_button(assigns) do
    ~H"""
    <%= if Livebook.Runtime.fixed_dependencies?(@runtime) do %>
      <span class="tooltip top" data-tooltip="The current runtime does not support adding dependencies">
        <button class="icon-button" disabled>
          <.remix_icon icon="play-list-add-line" class="text-xl" />
        </button>
      </span>
    <% else %>
      <span class="tooltip top" data-tooltip="Add dependency (sp)">
        <%= live_patch to: Routes.session_path(@socket, :package_search, @session_id),
              class: "icon-button",
              aria_label: "add dependency",
              role: "button",
              data_btn_package_search: true do %>
          <.remix_icon icon="play-list-add-line" class="text-xl" />
        <% end %>
      </span>
    <% end %>
    """
  end

  defp cell_link_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Link">
      <a href={"#cell-#{@cell_id}"} class="icon-button"
        role="button"
        aria-label="link to cell">
        <.remix_icon icon="link" class="text-xl" />
      </a>
    </span>
    """
  end

  def amplify_output_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Amplify output" data-el-amplify-outputs-button>
      <button class="icon-button" aria-label="amplify outputs">
        <.remix_icon icon="zoom-in-line" class="text-xl" />
      </button>
    </span>
    """
  end

  defp cell_settings_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Cell settings">
      <%= live_patch to: Routes.session_path(@socket, :cell_settings, @session_id, @cell_id),
            class: "icon-button",
            aria_label: "cell settings",
            role: "button" do %>
        <.remix_icon icon="settings-3-line" class="text-xl" />
      <% end %>
    </span>
    """
  end

  defp move_cell_up_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Move up">
      <button class="icon-button"
        aria-label="move cell up"
        phx-click="move_cell"
        phx-value-cell_id={@cell_id}
        phx-value-offset="-1">
        <.remix_icon icon="arrow-up-s-line" class="text-xl" />
      </button>
    </span>
    """
  end

  defp move_cell_down_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Move down">
      <button class="icon-button"
        aria-label="move cell down"
        phx-click="move_cell"
        phx-value-cell_id={@cell_id}
        phx-value-offset="1">
        <.remix_icon icon="arrow-down-s-line" class="text-xl" />
      </button>
    </span>
    """
  end

  defp delete_cell_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Delete">
      <button class="icon-button"
        aria-label="delete cell"
        phx-click={
          with_confirm(
            JS.push("delete_cell", value: %{cell_id: @cell_id}),
            title: "Delete cell",
            description: "Once you delete this cell, it will be moved to the bin.",
            confirm_text: "Delete",
            confirm_icon: "delete-bin-6-line",
            opt_out_id: "delete-cell"
          )
        }>
        <.remix_icon icon="delete-bin-6-line" class="text-xl" />
      </button>
    </span>
    """
  end

  defp setup_cell_info(assigns) do
    ~H"""
    <span class="tooltip left"
      data-tooltip={
        ~s'''
        The setup cell includes code that initializes the notebook
        and should run only once. This is the best place to install
        dependencies and set global configuration.\
        '''
      }>
      <span class="icon-button">
        <.remix_icon icon="question-line" class="text-xl" />
      </span>
    </span>
    """
  end

  defp evaluation_outputs(assigns) do
    ~H"""
    <div class="flex flex-col"
      data-el-outputs-container
      id={"outputs-#{@cell_view.id}-#{@cell_view.eval.outputs_batch_number}"}
      phx-update="append">
      <LivebookWeb.Output.outputs
        outputs={@cell_view.eval.outputs}
        dom_id_map={%{}}
        socket={@socket}
        session_id={@session_id}
        input_values={@cell_view.eval.input_values} />
    </div>
    """
  end

  defp empty?(%{source: ""} = _source_view), do: true
  defp empty?(_source_view), do: false

  defp cell_status(%{cell_view: %{eval: %{status: :evaluating}}} = assigns) do
    ~H"""
    <.status_indicator circle_class="bg-blue-500" animated_circle_class="bg-blue-400" change_indicator={true}>
      <span class="font-mono"
        id={"#{@id}-cell-timer"}
        phx-hook="Timer"
        phx-update="ignore"
        data-start={DateTime.to_iso8601(@cell_view.eval.evaluation_start)}>
      </span>
    </.status_indicator>
    """
  end

  defp cell_status(%{cell_view: %{eval: %{status: :queued}}} = assigns) do
    ~H"""
    <.status_indicator circle_class="bg-gray-400" animated_circle_class="bg-gray-300">
      Queued
    </.status_indicator>
    """
  end

  defp cell_status(%{cell_view: %{eval: %{validity: :evaluated}}} = assigns) do
    ~H"""
    <.status_indicator
      circle_class="bg-green-bright-400"
      change_indicator={true}
      tooltip={evaluated_label(@cell_view.eval.evaluation_time_ms)}>
      Evaluated
    </.status_indicator>
    """
  end

  defp cell_status(%{cell_view: %{eval: %{validity: :stale}}} = assigns) do
    ~H"""
    <.status_indicator circle_class="bg-yellow-bright-200" change_indicator={true}>
      Stale
    </.status_indicator>
    """
  end

  defp cell_status(%{cell_view: %{eval: %{validity: :aborted}}} = assigns) do
    ~H"""
    <.status_indicator circle_class="bg-gray-500">
      Aborted
    </.status_indicator>
    """
  end

  defp cell_status(assigns), do: ~H""

  defp status_indicator(assigns) do
    assigns =
      assigns
      |> assign_new(:animated_circle_class, fn -> nil end)
      |> assign_new(:change_indicator, fn -> false end)
      |> assign_new(:tooltip, fn -> nil end)

    ~H"""
    <div class={"#{if(@tooltip, do: "tooltip")} bottom distant-medium"} data-tooltip={@tooltip}>
      <div class="flex items-center space-x-1">
        <div class="flex text-xs text-gray-400" data-el-cell-status>
          <%= render_slot(@inner_block) %>
          <%= if @change_indicator do %>
            <span data-el-change-indicator>*</span>
          <% end %>
        </div>
        <span class="flex relative h-3 w-3">
          <%= if @animated_circle_class do %>
            <span class={"#{@animated_circle_class} animate-ping absolute inline-flex h-3 w-3 rounded-full opacity-75"}></span>
          <% end %>
          <span class={"#{@circle_class} relative inline-flex rounded-full h-3 w-3"}></span>
        </span>
      </div>
    </div>
    """
  end

  defp evaluated_label(time_ms) when is_integer(time_ms) do
    evaluation_time =
      if time_ms > 100 do
        seconds = time_ms |> Kernel./(1000) |> Float.floor(1)
        "#{seconds}s"
      else
        "#{time_ms}ms"
      end

    "Took " <> evaluation_time
  end

  defp evaluated_label(_time_ms), do: nil
end

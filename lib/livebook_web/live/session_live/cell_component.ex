defmodule LivebookWeb.SessionLive.CellComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, initialized: false)}
  end

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    socket =
      if not connected?(socket) or socket.assigns.initialized do
        socket
      else
        %{id: id, source_info: info} = socket.assigns.cell_view

        socket
        |> push_event("cell_init:#{id}", info)
        |> assign(initialized: true)
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col relative"
      data-element="cell"
      id={"cell-#{@cell_view.id}"}
      phx-hook="Cell"
      data-cell-id={@cell_view.id}
      data-focusable-id={@cell_view.id}
      data-type={@cell_view.type}
      data-session-path={Routes.session_path(@socket, :page, @session_id)}>
      <%= render_cell(assigns) %>
    </div>
    """
  end

  defp render_cell(%{cell_view: %{type: :markdown}} = assigns) do
    ~H"""
    <div class="mb-1 flex items-center justify-end">
      <div class="relative z-20 flex items-center justify-end space-x-2"
        role="toolbar"
        aria-label="cell actions"
        data-element="actions">
        <span class="tooltip top" data-tooltip="Edit content" data-element="enable-insert-mode-button">
          <button class="icon-button" aria-label="edit content">
            <.remix_icon icon="pencil-line" class="text-xl" />
          </button>
        </span>
        <span class="tooltip top" data-tooltip="Insert image" data-element="insert-image-button">
          <%= live_patch to: Routes.session_path(@socket, :cell_upload, @session_id, @cell_view.id),
                class: "icon-button",
                aria_label: "insert image",
                role: "button" do %>
            <.remix_icon icon="image-add-line" class="text-xl" />
          <% end %>
        </span>
        <.cell_link_button cell_id={@cell_view.id} />
        <.move_cell_up_button cell_id={@cell_view.id} />
        <.move_cell_down_button cell_id={@cell_view.id} />
        <.delete_cell_button cell_id={@cell_view.id} />
      </div>
    </div>

    <.cell_body>
      <div class="pb-4" data-element="editor-box">
        <.editor cell_view={@cell_view} />
      </div>
      <div class="markdown"
        data-element="markdown-container"
        id={"markdown-container-#{@cell_view.id}"}
        phx-update="ignore">
        <.content_placeholder bg_class="bg-gray-200" empty={empty?(@cell_view.source_info)} />
      </div>
    </.cell_body>
    """
  end

  defp render_cell(%{cell_view: %{type: :elixir}} = assigns) do
    ~H"""
    <div class="mb-1 flex items-center justify-between">
      <div class="relative z-20 flex items-center justify-end space-x-2" data-element="actions" data-primary>
        <%= if @cell_view.evaluation_status == :ready do %>
          <%= if @cell_view.validity_status != :fresh and @cell_view.reevaluate_automatically do %>
            <%= live_patch to: Routes.session_path(@socket, :cell_settings, @session_id, @cell_view.id),
                  class: "text-gray-600 hover:text-gray-800 focus:text-gray-800 flex space-x-1 items-center" do %>
              <.remix_icon icon="check-line" class="text-xl" />
              <span class="text-sm font-medium">
                Reevaluates automatically
              </span>
            <% end %>
          <% else %>
            <button class="text-gray-600 hover:text-gray-800 focus:text-gray-800 flex space-x-1 items-center"
              phx-click="queue_cell_evaluation"
              phx-value-cell_id={@cell_view.id}>
              <.remix_icon icon="play-circle-fill" class="text-xl" />
              <span class="text-sm font-medium">
                <%= if(@cell_view.validity_status == :evaluated, do: "Reevaluate", else: "Evaluate") %>
              </span>
            </button>
          <% end %>
        <% else %>
          <button class="text-gray-600 hover:text-gray-800 focus:text-gray-800 flex space-x-1 items-center"
            phx-click="cancel_cell_evaluation"
            phx-value-cell_id={@cell_view.id}>
            <.remix_icon icon="stop-circle-fill" class="text-xl" />
            <span class="text-sm font-medium">
              Stop
            </span>
          </button>
        <% end %>
      </div>
      <div class="relative z-20 flex items-center justify-end space-x-2"
        role="toolbar"
        aria-label="cell actions"
        data-element="actions">
        <span class="tooltip top" data-tooltip="Amplify output" data-element="amplify-outputs-button">
          <button class="icon-button"
            aria-label="amplify outputs">
            <.remix_icon icon="zoom-in-line" class="text-xl" data-element="zoom-in-icon" />
            <.remix_icon icon="zoom-out-line" class="text-xl" data-element="zoom-out-icon" />
          </button>
        </span>
        <.cell_settings_button cell_id={@cell_view.id} socket={@socket} session_id={@session_id} />
        <.cell_link_button cell_id={@cell_view.id} />
        <.move_cell_up_button cell_id={@cell_view.id} />
        <.move_cell_down_button cell_id={@cell_view.id} />
        <.delete_cell_button cell_id={@cell_view.id} />
      </div>
    </div>

    <.cell_body>
      <.editor cell_view={@cell_view} />
      <div class="flex flex-col"
        aria-live="polite"
        data-element="outputs-container"
        id={"outputs-#{@cell_view.id}-#{@cell_view.outputs_batch_number}"}
        phx-update="append">
        <LivebookWeb.Output.outputs
          outputs={@cell_view.outputs}
          dom_id_map={%{}}
          socket={@socket}
          session_id={@session_id}
          runtime={@runtime}
          cell_validity_status={@cell_view.validity_status}
          input_values={@cell_view.input_values} />
      </div>
    </.cell_body>
    """
  end

  defp cell_body(assigns) do
    ~H"""
    <!-- By setting tabindex we can programmatically focus this element,
         also we actually want to make this element tab-focusable -->
    <div class="flex relative" data-element="cell-body" tabindex="0">
      <div class="w-1 h-full rounded-lg absolute top-0 -left-3" data-element="cell-focus-indicator">
      </div>
      <div class="w-full">
        <%= render_slot(@inner_block) %>
      </div>
    </div>
    """
  end

  defp cell_link_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Link">
      <a href={"#cell-#{@cell_id}"} class="icon-button"
        aria-label="link to cell">
        <.remix_icon icon="link" class="text-xl" />
      </a>
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
        phx-click="delete_cell"
        phx-value-cell_id={@cell_id}>
        <.remix_icon icon="delete-bin-6-line" class="text-xl" />
      </button>
    </span>
    """
  end

  defp editor(assigns) do
    ~H"""
    <div class="relative">
      <div id={"editor-#{@cell_view.id}"} phx-update="ignore">
        <div class="py-3 rounded-lg bg-editor" data-element="editor-container">
          <div class="px-8">
            <.content_placeholder bg_class="bg-gray-500" empty={empty?(@cell_view.source_info)} />
          </div>
        </div>
      </div>

      <%= if @cell_view.type == :elixir do %>
        <div class="absolute bottom-2 right-2">
          <.cell_status cell_view={@cell_view} />
        </div>
      <% end %>
    </div>
    """
  end

  # The whole page has to load and then hooks are mounted.
  # There may be a tiny delay before the markdown is rendered
  # or editors are mounted, so show neat placeholders immediately.

  defp content_placeholder(assigns) do
    ~H"""
    <%= if @empty do %>
      <div class="h-4"></div>
    <% else %>
      <div class="max-w-2xl w-full animate-pulse">
        <div class="flex-1 space-y-4">
          <div class={"#{@bg_class} h-4 rounded-lg w-3/4"}></div>
          <div class={"#{@bg_class} h-4 rounded-lg"}></div>
          <div class={"#{@bg_class} h-4 rounded-lg w-5/6"}></div>
        </div>
      </div>
    <% end %>
    """
  end

  defp empty?(%{source: ""} = _source_info), do: true
  defp empty?(_source_info), do: false

  defp cell_status(%{cell_view: %{evaluation_status: :evaluating}} = assigns) do
    ~H"""
    <.status_indicator circle_class="bg-blue-500" animated_circle_class="bg-blue-400" change_indicator={true}>
      <span class="font-mono"
        id={"cell-timer-#{@cell_view.id}-evaluation-#{@cell_view.evaluation_number}"}
        phx-hook="Timer"
        phx-update="ignore"
        data-start={DateTime.to_iso8601(@cell_view.evaluation_start)}>
      </span>
    </.status_indicator>
    """
  end

  defp cell_status(%{cell_view: %{evaluation_status: :queued}} = assigns) do
    ~H"""
    <.status_indicator circle_class="bg-gray-400" animated_circle_class="bg-gray-300">
      Queued
    </.status_indicator>
    """
  end

  defp cell_status(%{cell_view: %{validity_status: :evaluated}} = assigns) do
    ~H"""
    <.status_indicator
      circle_class="bg-green-bright-400"
      change_indicator={true}
      tooltip={evaluated_label(@cell_view.evaluation_time_ms)}>
      Evaluated
    </.status_indicator>
    """
  end

  defp cell_status(%{cell_view: %{validity_status: :stale}} = assigns) do
    ~H"""
    <.status_indicator circle_class="bg-yellow-bright-200" change_indicator={true}>
      Stale
    </.status_indicator>
    """
  end

  defp cell_status(%{cell_view: %{validity_status: :aborted}} = assigns) do
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
        <div class="flex text-xs text-gray-400" data-element="cell-status">
          <%= render_slot(@inner_block) %>
          <%= if @change_indicator do %>
            <span data-element="change-indicator">*</span>
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

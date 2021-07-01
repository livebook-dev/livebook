defmodule LivebookWeb.SessionLive.CellComponent do
  use LivebookWeb, :live_component

  def render(assigns) do
    ~L"""
    <div class="flex flex-col relative"
      data-element="cell"
      id="cell-<%= @cell_view.id %>"
      phx-hook="Cell"
      data-cell-id="<%= @cell_view.id %>"
      data-type="<%= @cell_view.type %>"
      data-session-path="<%= Routes.session_path(@socket, :page, @session_id) %>">
      <%= render_cell_content(assigns) %>
    </div>
    """
  end

  def render_cell_content(%{cell_view: %{type: :markdown}} = assigns) do
    ~L"""
    <div class="mb-1 flex items-center justify-end">
      <div class="relative z-20 flex items-center justify-end space-x-2" data-element="actions">
        <%= render_cell_anchor_link(assigns) %>
        <span class="tooltip top" aria-label="Edit content" data-element="enable-insert-mode-button">
          <button class="icon-button">
            <%= remix_icon("pencil-line", class: "text-xl") %>
          </button>
        </span>
        <span class="tooltip top" aria-label="Insert image" data-element="insert-image-button">
          <%= live_patch to: Routes.session_path(@socket, :cell_upload, @session_id, @cell_view.id),
                class: "icon-button" do %>
            <%= remix_icon("image-add-line", class: "text-xl") %>
          <% end %>
        </span>
        <span class="tooltip top" aria-label="Move up">
          <button class="icon-button"
            phx-click="move_cell"
            phx-value-cell_id="<%= @cell_view.id %>"
            phx-value-offset="-1">
            <%= remix_icon("arrow-up-s-line", class: "text-xl") %>
          </button>
        </span>
        <span class="tooltip top" aria-label="Move down">
          <button class="icon-button"
            phx-click="move_cell"
            phx-value-cell_id="<%= @cell_view.id %>"
            phx-value-offset="1">
            <%= remix_icon("arrow-down-s-line", class: "text-xl") %>
          </button>
        </span>
        <span class="tooltip top" aria-label="Delete">
          <button class="icon-button"
            phx-click="delete_cell"
            phx-value-cell_id="<%= @cell_view.id %>">
            <%= remix_icon("delete-bin-6-line", class: "text-xl") %>
          </button>
        </span>
      </div>
    </div>

    <div class="flex relative">
      <div class="w-1 h-full rounded-lg absolute top-0 -left-3" data-element="cell-focus-indicator">
      </div>
      <div class="w-full">
        <div class="pb-4" data-element="editor-box">
          <%= render_editor(assigns) %>
        </div>

        <div class="markdown" data-element="markdown-container" id="markdown-container-<%= @cell_view.id %>" phx-update="ignore">
          <%= render_content_placeholder("bg-gray-200", @cell_view.empty?) %>
        </div>
      </div>
    </div>
    """
  end

  def render_cell_content(%{cell_view: %{type: :elixir}} = assigns) do
    ~L"""
    <div class="mb-1 flex items-center justify-between">
      <div class="relative z-20 flex items-center justify-end space-x-2" data-element="actions" data-primary>
        <%= if @cell_view.evaluation_status == :ready do %>
          <button class="text-gray-600 hover:text-gray-800 focus:text-gray-800 flex space-x-1 items-center"
            phx-click="queue_cell_evaluation"
            phx-value-cell_id="<%= @cell_view.id %>">
            <%= remix_icon("play-circle-fill", class: "text-xl") %>
            <span class="text-sm font-medium">
              <%= if(@cell_view.validity_status == :evaluated, do: "Reevaluate", else: "Evaluate") %>
            </span>
          </button>
        <% else %>
          <button class="text-gray-600 hover:text-gray-800 focus:text-gray-800 flex space-x-1 items-center"
            phx-click="cancel_cell_evaluation"
            phx-value-cell_id="<%= @cell_view.id %>">
            <%= remix_icon("stop-circle-fill", class: "text-xl") %>
            <span class="text-sm font-medium">
              Stop
            </span>
          </button>
        <% end %>
      </div>
      <div class="relative z-20 flex items-center justify-end space-x-2" data-element="actions">
        <%= render_cell_anchor_link(assigns) %>
        <span class="tooltip top" aria-label="Cell settings">
          <%= live_patch to: Routes.session_path(@socket, :cell_settings, @session_id, @cell_view.id), class: "icon-button" do %>
            <%= remix_icon("list-settings-line", class: "text-xl") %>
          <% end %>
        </span>
        <span class="tooltip top" aria-label="Move up">
          <button class="icon-button"
            phx-click="move_cell"
            phx-value-cell_id="<%= @cell_view.id %>"
            phx-value-offset="-1">
            <%= remix_icon("arrow-up-s-line", class: "text-xl") %>
          </button>
        </span>
        <span class="tooltip top" aria-label="Move down">
          <button class="icon-button"
            phx-click="move_cell"
            phx-value-cell_id="<%= @cell_view.id %>"
            phx-value-offset="1">
            <%= remix_icon("arrow-down-s-line", class: "text-xl") %>
          </button>
        </span>
        <span class="tooltip top" aria-label="Delete">
          <button class="icon-button"
            phx-click="delete_cell"
            phx-value-cell_id="<%= @cell_view.id %>">
            <%= remix_icon("delete-bin-6-line", class: "text-xl") %>
          </button>
        </span>
      </div>
    </div>

    <div class="flex relative">
      <div class="w-1 h-full rounded-lg absolute top-0 -left-3" data-element="cell-focus-indicator">
      </div>
      <div class="w-full">
        <%= render_editor(assigns) %>

        <%= if @cell_view.outputs != [] do %>
          <div class="mt-2">
            <%= render_outputs(assigns, @socket) %>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  def render_cell_content(%{cell_view: %{type: :input}} = assigns) do
    ~L"""
    <div class="mb-1 flex items-center justify-end">
      <div class="relative z-20 flex items-center justify-end space-x-2" data-element="actions">
        <%= render_cell_anchor_link(assigns) %>
        <span class="tooltip top" aria-label="Cell settings">
          <%= live_patch to: Routes.session_path(@socket, :cell_settings, @session_id, @cell_view.id), class: "icon-button" do %>
            <%= remix_icon("list-settings-line", class: "text-xl") %>
          <% end %>
        </span>
        <span class="tooltip top" aria-label="Move up">
          <button class="icon-button"
            phx-click="move_cell"
            phx-value-cell_id="<%= @cell_view.id %>"
            phx-value-offset="-1">
            <%= remix_icon("arrow-up-s-line", class: "text-xl") %>
          </button>
        </span>
        <span class="tooltip top" aria-label="Move down">
          <button class="icon-button"
            phx-click="move_cell"
            phx-value-cell_id="<%= @cell_view.id %>"
            phx-value-offset="1">
            <%= remix_icon("arrow-down-s-line", class: "text-xl") %>
          </button>
        </span>
        <span class="tooltip top" aria-label="Delete">
          <button class="icon-button"
            phx-click="delete_cell"
            phx-value-cell_id="<%= @cell_view.id %>">
            <%= remix_icon("delete-bin-6-line", class: "text-xl") %>
          </button>
        </span>
      </div>
    </div>

    <div class="flex relative">
      <div class="w-1 h-full rounded-lg absolute top-0 -left-3" data-element="cell-focus-indicator">
      </div>
      <div>
        <form phx-change="set_cell_value" phx-submit="queue_bound_cells_evaluation">
          <input type="hidden" name="cell_id" value="<%= @cell_view.id %>" />
          <div class="input-label">
            <%= @cell_view.name %>
          </div>

          <%= if (@cell_view.input_type == :textarea) do %>
            <textarea
              data-element="input"
              class="input <%= if(@cell_view.error, do: "input--error") %>"
              name="value"
              spellcheck="false"
              tabindex="-1"><%= [?\n, @cell_view.value] %></textarea>
          <% else %>
            <input type="<%= html_input_type(@cell_view.input_type) %>"
              data-element="input"
              class="input <%= if(@cell_view.error, do: "input--error") %>"
              name="value"
              value="<%= @cell_view.value %>"
              phx-debounce="300"
              spellcheck="false"
              autocomplete="off"
              tabindex="-1" />
          <% end %>

          <%= if @cell_view.error do %>
            <div class="input-error">
              <%= String.capitalize(@cell_view.error) %>
            </div>
          <% end %>
        </form>
      </div>
    </div>
    """
  end

  defp render_editor(assigns) do
    ~L"""
    <div class="py-3 rounded-lg bg-editor relative">
      <div
        id="editor-container-<%= @cell_view.id %>"
        data-element="editor-container"
        phx-update="ignore">
        <div class="px-8">
          <%= render_content_placeholder("bg-gray-500", @cell_view.empty?) %>
        </div>
      </div>

      <%= if @cell_view.type == :elixir do %>
        <div class="absolute bottom-2 right-2">
          <%= render_cell_status(
                @cell_view.validity_status,
                @cell_view.evaluation_status,
                @cell_view.evaluation_time_ms,
                "cell-#{@cell_view.id}-evaluation#{@cell_view.number_of_evaluations}"
              ) %>
        </div>
      <% end %>
    </div>
    """
  end

  defp render_cell_anchor_link(assigns) do
    ~L"""
    <span class="tooltip top" aria-label="Link">
      <a href="#cell-<%= @cell_view.id %>" class="icon-button">
        <%= remix_icon("link", class: "text-xl") %>
      </a>
    </span>
    """
  end

  # The whole page has to load and then hooks are mounded.
  # There may be a tiny delay before the markdown is rendered
  # or editors are mounted, so show neat placeholders immediately.

  defp render_content_placeholder(_bg_class, true = _empty) do
    assigns = %{}

    ~L"""
    <div class="h-4"></div>
    """
  end

  defp render_content_placeholder(bg_class, false = _empty) do
    assigns = %{bg_class: bg_class}

    ~L"""
    <div class="max-w-2xl w-full animate-pulse">
      <div class="flex-1 space-y-4">
        <div class="h-4 <%= @bg_class %> rounded-lg w-3/4"></div>
        <div class="h-4 <%= @bg_class %> rounded-lg"></div>
        <div class="h-4 <%= @bg_class %> rounded-lg w-5/6"></div>
      </div>
    </div>
    """
  end

  defp render_outputs(assigns, socket) do
    ~L"""
    <div class="flex flex-col rounded-lg border border-gray-200 divide-y divide-gray-200">
      <%= for {output, index} <- @cell_view.outputs |> Enum.reverse() |> Enum.with_index(), output != :ignored do %>
        <div class="p-4 max-w-full overflow-y-auto tiny-scrollbar">
          <%= render_output(socket, output, "cell-#{@cell_view.id}-output#{index}") %>
        </div>
      <% end %>
    </div>
    """
  end

  defp render_output(_socket, text, id) when is_binary(text) do
    # Captured output usually has a trailing newline that we can ignore,
    # because each line is itself an HTML block anyway.
    text = String.replace_suffix(text, "\n", "")
    live_component(LivebookWeb.Output.TextComponent, id: id, content: text, follow: true)
  end

  defp render_output(_socket, {:text, text}, id) do
    live_component(LivebookWeb.Output.TextComponent, id: id, content: text, follow: false)
  end

  defp render_output(_socket, {:markdown, markdown}, id) do
    live_component(LivebookWeb.Output.MarkdownComponent, id: id, content: markdown)
  end

  defp render_output(_socket, {:image, content, mime_type}, id) do
    live_component(LivebookWeb.Output.ImageComponent,
      id: id,
      content: content,
      mime_type: mime_type
    )
  end

  defp render_output(_socket, {:vega_lite_static, spec}, id) do
    live_component(LivebookWeb.Output.VegaLiteStaticComponent, id: id, spec: spec)
  end

  defp render_output(socket, {:vega_lite_dynamic, pid}, id) do
    live_render(socket, LivebookWeb.Output.VegaLiteDynamicLive,
      id: id,
      session: %{"id" => id, "pid" => pid}
    )
  end

  defp render_output(socket, {:table_dynamic, pid}, id) do
    live_render(socket, LivebookWeb.Output.TableDynamicLive,
      id: id,
      session: %{"id" => id, "pid" => pid}
    )
  end

  defp render_output(_socket, {:error, formatted, :runtime_restart_required}, _id) do
    assigns = %{formatted: formatted}

    ~L"""
    <div class="flex flex-col space-y-4">
      <%= render_error_message_output(@formatted) %>
      <div>
        <button class="button button-gray" phx-click="restart_runtime">
          Restart runtime
        </button>
      </div>
    </div>
    """
  end

  defp render_output(_socket, {:error, formatted, _type}, _id) do
    render_error_message_output(formatted)
  end

  defp render_output(_socket, output, _id) do
    render_error_message_output("""
    Unknown output format: #{inspect(output)}. If you're using Kino,
    you may want to update Kino and Livebook to the latest version.
    """)
  end

  defp render_error_message_output(message) do
    assigns = %{message: message}

    ~L"""
    <div class="overflow-auto whitespace-pre text-red-600 tiny-scrollbar"><%= @message %></div>
    """
  end

  defp render_cell_status(cell_view, evaluation_status, evaluation_time_ms, evaluation_id)

  defp render_cell_status(_, :evaluating, _, evaluation_id) do
    timer =
      content_tag(:span, nil,
        phx_hook: "Timer",
        # Make sure each evaluation gets its own timer
        id: "#{evaluation_id}-timer",
        phx_update: "ignore",
        class: "font-mono"
      )

    render_status_indicator(timer, "bg-blue-500",
      animated_circle_class: "bg-blue-400",
      change_indicator: true
    )
  end

  defp render_cell_status(_, :queued, _, _) do
    render_status_indicator("Queued", "bg-gray-500", animated_circle_class: "bg-gray-400")
  end

  defp render_cell_status(:evaluated, _, evaluation_time_ms, _) do
    render_status_indicator("Evaluated", "bg-green-400",
      change_indicator: true,
      tooltip: evaluated_label(evaluation_time_ms)
    )
  end

  defp render_cell_status(:stale, _, evaluation_time_ms, _) do
    render_status_indicator("Stale", "bg-yellow-200",
      change_indicator: true,
      tooltip: evaluated_label(evaluation_time_ms)
    )
  end

  defp render_cell_status(:aborted, _, _, _) do
    render_status_indicator("Aborted", "bg-red-400")
  end

  defp render_cell_status(_, _, _, _), do: nil

  defp render_status_indicator(element, circle_class, opts \\ []) do
    assigns = %{
      element: element,
      circle_class: circle_class,
      animated_circle_class: Keyword.get(opts, :animated_circle_class),
      change_indicator: Keyword.get(opts, :change_indicator, false),
      tooltip: Keyword.get(opts, :tooltip)
    }

    ~L"""
    <div class="<%= if(@tooltip, do: "tooltip") %> bottom distant-medium" aria-label="<%= @tooltip %>">
      <div class="flex items-center space-x-1">
        <div class="flex text-xs text-gray-400">
          <%= @element %>
          <%= if @change_indicator do %>
            <span data-element="change-indicator">*</span>
          <% end %>
        </div>
        <span class="flex relative h-3 w-3">
          <%= if @animated_circle_class do %>
            <span class="animate-ping absolute inline-flex h-3 w-3 rounded-full <%= @animated_circle_class %> opacity-75"></span>
          <% end %>
          <span class="relative inline-flex rounded-full h-3 w-3 <%= @circle_class %>"></span>
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

  defp html_input_type(:password), do: "password"
  defp html_input_type(:number), do: "number"
  defp html_input_type(:color), do: "color"
  defp html_input_type(_), do: "text"
end

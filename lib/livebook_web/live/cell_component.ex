defmodule LivebookWeb.CellComponent do
  use LivebookWeb, :live_component

  def render(assigns) do
    ~L"""
    <div class="flex flex-col relative"
      data-element="cell"
      id="cell-<%= @cell.id %>"
      phx-hook="Cell"
      data-cell-id="<%= @cell.id %>"
      data-type="<%= @cell.type %>">
      <%= render_cell_content(assigns) %>
    </div>
    """
  end

  def render_cell_content(%{cell: %{type: :markdown}} = assigns) do
    ~L"""
    <div class="mb-1 flex items-center justify-end" data-element="actions">
      <div class="relative z-10 flex items-center justify-end space-x-2">
        <button data-element="enable-insert-mode-button">
          <%= remix_icon("pencil-line", class: "text-xl action-icon") %>
        </button>
        <button phx-click="move_cell"
          phx-value-cell_id="<%= @cell.id %>"
          phx-value-offset="-1">
          <%= remix_icon("arrow-up-s-line", class: "text-xl action-icon") %>
        </button>
        <button phx-click="move_cell"
          phx-value-cell_id="<%= @cell.id %>"
          phx-value-offset="1">
          <%= remix_icon("arrow-down-s-line", class: "text-xl action-icon") %>
        </button>
        <button phx-click="delete_cell"
          phx-value-cell_id="<%= @cell.id %>">
          <%= remix_icon("delete-bin-6-line", class: "text-xl action-icon") %>
        </button>
      </div>
    </div>

    <div class="flex">
      <div class="w-1 rounded-lg relative -left-3" data-element="cell-focus-indicator">
      </div>
      <div class="w-full">
        <div class="pb-4" data-element="editor-box">
          <%= render_editor(@cell, @cell_info) %>
        </div>

        <div class="markdown" data-element="markdown-container" id="markdown-container-<%= @cell.id %>" phx-update="ignore">
          <%= render_markdown_content_placeholder(@cell.source) %>
        </div>
      </div>
    </div>
    """
  end

  def render_cell_content(%{cell: %{type: :elixir}} = assigns) do
    ~L"""
    <div class="mb-1 flex justify-between" data-element="actions">
      <div class="relative z-10 flex items-center justify-end space-x-2" data-element="primary-actions">
        <%= if @cell_info.evaluation_status == :ready do %>
          <button class="text-gray-600 hover:text-gray-800 flex space-x-1 items-center"
            phx-click="queue_cell_evaluation"
            phx-value-cell_id="<%= @cell.id %>">
            <%= remix_icon("play-circle-fill", class: "text-xl") %>
            <span class="text-sm font-medium">
              <%= if(@cell_info.validity_status == :evaluated, do: "Reevaluate", else: "Evaluate") %>
            </span>
          </button>
        <% else %>
          <button class="text-gray-600 hover:text-gray-800 flex space-x-1 items-center"
            phx-click="cancel_cell_evaluation"
            phx-value-cell_id="<%= @cell.id %>">
            <%= remix_icon("stop-circle-fill", class: "text-xl") %>
            <span class="text-sm font-medium">
              Stop
            </span>
          </button>
        <% end %>
      </div>
      <div class="relative z-10 flex items-center justify-end space-x-2">
        <%= live_patch to: Routes.session_path(@socket, :cell_settings, @session_id, @cell.id) do %>
          <%= remix_icon("list-settings-line", class: "text-xl action-icon") %>
        <% end %>
        <button phx-click="move_cell"
          phx-value-cell_id="<%= @cell.id %>"
          phx-value-offset="-1">
          <%= remix_icon("arrow-up-s-line", class: "text-xl action-icon") %>
        </button>
        <button phx-click="move_cell"
          phx-value-cell_id="<%= @cell.id %>"
          phx-value-offset="1">
          <%= remix_icon("arrow-down-s-line", class: "text-xl action-icon") %>
        </button>
        <button phx-click="delete_cell"
          phx-value-cell_id="<%= @cell.id %>">
          <%= remix_icon("delete-bin-6-line", class: "text-xl action-icon") %>
        </button>
      </div>
    </div>

    <div class="flex">
      <div class="w-1 rounded-lg relative -left-3" data-element="cell-focus-indicator">
      </div>
      <div class="w-full">
        <%= render_editor(@cell, @cell_info, show_status: true) %>

        <%= if @cell.outputs != [] do %>
          <div class="mt-2">
            <%= render_outputs(@cell.outputs, @cell.id) %>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  defp render_editor(cell, cell_info, opts \\ []) do
    show_status = Keyword.get(opts, :show_status, false)
    assigns = %{cell: cell, cell_info: cell_info, show_status: show_status}

    ~L"""
    <div class="py-3 rounded-lg overflow-hidden bg-editor relative">
      <div
        id="editor-container-<%= @cell.id %>"
        data-element="editor-container"
        phx-update="ignore">
        <%= render_editor_content_placeholder(@cell.source) %>
      </div>

      <%= if @show_status do %>
        <div class="absolute bottom-2 right-2">
          <%= render_cell_status(@cell_info) %>
        </div>
      <% end %>
    </div>
    """
  end

  # The whole page has to load and then hooks are mounded.
  # There may be a tiny delay before the markdown is rendered
  # or and editors are mounted, so show neat placeholders immediately.

  defp render_markdown_content_placeholder("" = _content) do
    assigns = %{}

    ~L"""
    <div class="h-4"></div>
    """
  end

  defp render_markdown_content_placeholder(_content) do
    assigns = %{}

    ~L"""
    <div class="max-w-2xl w-full animate-pulse">
      <div class="flex-1 space-y-4">
        <div class="h-4 bg-gray-200 rounded-lg w-3/4"></div>
        <div class="h-4 bg-gray-200 rounded-lg"></div>
        <div class="h-4 bg-gray-200 rounded-lg w-5/6"></div>
      </div>
    </div>
    """
  end

  defp render_editor_content_placeholder("" = _content) do
    assigns = %{}

    ~L"""
    <div class="h-4"></div>
    """
  end

  defp render_editor_content_placeholder(_content) do
    assigns = %{}

    ~L"""
    <div class="px-8 max-w-2xl w-full animate-pulse">
      <div class="flex-1 space-y-4 py-1">
        <div class="h-4 bg-gray-500 rounded-lg w-3/4"></div>
        <div class="h-4 bg-gray-500 rounded-lg"></div>
        <div class="h-4 bg-gray-500 rounded-lg w-5/6"></div>
      </div>
    </div>
    """
  end

  defp render_outputs(outputs, cell_id) do
    assigns = %{outputs: outputs, cell_id: cell_id}

    ~L"""
    <div class="flex flex-col rounded-lg border border-gray-200 divide-y divide-gray-200 font-editor">
      <%= for {output, index} <- @outputs |> Enum.reverse() |> Enum.with_index(), output != :ignored do %>
        <div class="p-4">
          <%= render_output(output, "#{@cell_id}-output#{index}") %>
        </div>
      <% end %>
    </div>
    """
  end

  defp render_output(output, id) when is_binary(output) do
    lines = ansi_to_html_lines(output)
    assigns = %{lines: lines, id: id}

    ~L"""
    <div id="<%= @id %>" phx-hook="VirtualizedLines" data-max-height="300">
      <div data-template class="hidden"><%= for line <- @lines do %><div><%= raw line %></div><% end %></div>
      <div data-content phx-update="ignore" class="overflow-auto whitespace-pre text-gray-500 tiny-scrollbar"></div>
    </div>
    """
  end

  defp render_output({:inspect, inspected}, id) do
    lines = ansi_to_html_lines(inspected)
    assigns = %{lines: lines, id: id}

    ~L"""
    <div id="<%= @id %>" phx-hook="VirtualizedLines" data-max-height="300">
      <div data-template class="hidden"><%= for line <- @lines do %><div><%= raw line %></div><% end %></div>
      <div data-content phx-update="ignore" class="overflow-auto whitespace-pre text-gray-500 tiny-scrollbar"></div>
    </div>
    """
  end

  defp render_output({:error, formatted}, _id) do
    assigns = %{formatted: formatted}

    ~L"""
    <div class="overflow-auto whitespace-pre text-red-600 tiny-scrollbar"><%= @formatted %></div>
    """
  end

  defp ansi_to_html_lines(string) do
    string
    |> ansi_string_to_html(
      # Make sure every line is styled separately,
      # so tht later we can safely split the whole HTML
      # into valid HTML lines.
      renderer: fn style, content ->
        content
        |> IO.iodata_to_binary()
        |> String.split("\n")
        |> Enum.map(&[~s{<span style="#{style}">}, &1, ~s{</span>}])
        |> Enum.intersperse("\n")
      end
    )
    |> Phoenix.HTML.safe_to_string()
    |> String.split("\n")
  end

  defp render_cell_status(%{evaluation_status: :evaluating} = info) do
    render_status_indicator(
      "Evaluating",
      "bg-blue-500",
      "bg-blue-400",
      info.digest != info.evaluation_digest
    )
  end

  defp render_cell_status(%{evaluation_status: :queued}) do
    render_status_indicator("Queued", "bg-gray-500", "bg-gray-400", false)
  end

  defp render_cell_status(%{validity_status: :evaluated} = info) do
    render_status_indicator(
      "Evaluated",
      "bg-green-400",
      nil,
      info.digest != info.evaluation_digest
    )
  end

  defp render_cell_status(%{validity_status: :stale} = info) do
    render_status_indicator("Stale", "bg-yellow-200", nil, info.digest != info.evaluation_digest)
  end

  defp render_cell_status(_), do: nil

  defp render_status_indicator(text, circle_class, animated_circle_class, show_changed) do
    assigns = %{
      text: text,
      circle_class: circle_class,
      animated_circle_class: animated_circle_class,
      show_changed: show_changed
    }

    ~L"""
    <div class="flex items-center space-x-1">
      <div class="flex text-xs text-gray-400">
        <%= @text %>
        <span class="<%= unless(@show_changed, do: "invisible") %>">*</span>
      </div>
      <span class="flex relative h-3 w-3">
        <span class="animate-ping absolute inline-flex h-3 w-3 rounded-full <%= @animated_circle_class %> opacity-75"></span>
        <span class="relative inline-flex rounded-full h-3 w-3 <%= @circle_class %>"></span>
      </span>
    </div>
    """
  end
end

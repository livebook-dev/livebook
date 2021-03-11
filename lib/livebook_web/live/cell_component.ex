defmodule LivebookWeb.CellComponent do
  use LivebookWeb, :live_component

  def render(assigns) do
    ~L"""
    <div class="cell flex flex-col relative mr-10 border-l-4 pl-4 -ml-4 border-blue-100 border-opacity-0 hover:border-opacity-100"
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
    <div class="flex flex-col items-center space-y-2 absolute z-50 right-0 top-0 -mr-10" data-element="actions">
      <button class="text-gray-500 hover:text-current" data-element="enable-insert-mode-button">
        <%= remix_icon("pencil-line", class: "text-2xl") %>
      </button>
      <button class="text-gray-500 hover:text-current"
        phx-click="delete_cell"
        phx-value-cell_id="<%= @cell.id %>">
        <%= remix_icon("delete-bin-line", class: "text-2xl") %>
      </button>
      <button class="text-gray-500 hover:text-current"
        phx-click="move_cell"
        phx-value-cell_id="<%= @cell.id %>"
        phx-value-offset="-1">
        <%= remix_icon("arrow-up-s-line", class: "text-2xl") %>
      </button>
      <button class="text-gray-500 hover:text-current"
        phx-click="move_cell"
        phx-value-cell_id="<%= @cell.id %>"
        phx-value-offset="1">
        <%= remix_icon("arrow-down-s-line", class: "text-2xl") %>
      </button>
    </div>

    <div class="pb-4" data-element="editor-box">
      <%= render_editor(@cell, @cell_info) %>
    </div>

    <div class="markdown" data-element="markdown-container" id="markdown-container-<%= @cell.id %>" phx-update="ignore">
      <%= render_markdown_content_placeholder(@cell.source) %>
    </div>
    """
  end

  def render_cell_content(%{cell: %{type: :elixir}} = assigns) do
    ~L"""
    <div class="flex flex-col items-center space-y-2 absolute z-50 right-0 top-0 -mr-10" data-element="actions">
      <%= if @cell_info.evaluation_status == :ready do %>
        <button class="text-gray-500 hover:text-current"
          phx-click="queue_cell_evaluation"
          phx-value-cell_id="<%= @cell.id %>">
          <%= remix_icon("play-circle-line", class: "text-2xl") %>
        </button>
      <% else %>
        <button class="text-gray-500 hover:text-current"
          phx-click="cancel_cell_evaluation"
          phx-value-cell_id="<%= @cell.id %>">
          <%= remix_icon("stop-circle-line", class: "text-2xl") %>
        </button>
      <% end %>
      <button class="text-gray-500 hover:text-current"
        phx-click="delete_cell"
        phx-value-cell_id="<%= @cell.id %>">
        <%= remix_icon("delete-bin-line", class: "text-2xl") %>
      </button>
      <%= live_patch to: Routes.session_path(@socket, :cell_settings, @session_id, @cell.id), class: "text-gray-500 hover:text-current" do %>
        <%= remix_icon("list-settings-line", class: "text-2xl") %>
      <% end %>
      <button class="text-gray-500 hover:text-current"
        phx-click="move_cell"
        phx-value-cell_id="<%= @cell.id %>"
        phx-value-offset="-1">
        <%= remix_icon("arrow-up-s-line", class: "text-2xl") %>
      </button>
      <button class="text-gray-500 hover:text-current"
        phx-click="move_cell"
        phx-value-cell_id="<%= @cell.id %>"
        phx-value-offset="1">
        <%= remix_icon("arrow-down-s-line", class: "text-2xl") %>
      </button>
    </div>

    <%= render_editor(@cell, @cell_info, show_status: true) %>

    <%= if @cell.outputs != [] do %>
      <div class="mt-2">
        <%= render_outputs(@cell.outputs, @cell.id) %>
      </div>
    <% end %>
    """
  end

  defp render_editor(cell, cell_info, opts \\ []) do
    show_status = Keyword.get(opts, :show_status, false)
    assigns = %{cell: cell, cell_info: cell_info, show_status: show_status}

    ~L"""
    <div class="py-3 rounded-md overflow-hidden bg-editor relative">
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
        <div class="h-4 bg-gray-200 rounded-md w-3/4"></div>
        <div class="h-4 bg-gray-200 rounded-md"></div>
        <div class="h-4 bg-gray-200 rounded-md w-5/6"></div>
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
        <div class="h-4 bg-gray-500 rounded-md w-3/4"></div>
        <div class="h-4 bg-gray-500 rounded-md"></div>
        <div class="h-4 bg-gray-500 rounded-md w-5/6"></div>
      </div>
    </div>
    """
  end

  defp render_outputs(outputs, cell_id) do
    assigns = %{outputs: outputs, cell_id: cell_id}

    ~L"""
    <div class="flex flex-col rounded-md border border-gray-200 divide-y divide-gray-200 font-editor">
      <%= for {output, index} <- @outputs |> Enum.reverse() |> Enum.with_index() do %>
        <div class="p-4">
          <div class="">
            <%= render_output(output, "#{@cell_id}-output#{index}") %>
          </div>
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
    <div class="whitespace-pre text-red-600"><%= @formatted %></div>
    """
  end

  defp ansi_to_html_lines(string) do
    string
    |> ansi_string_to_html()
    |> Phoenix.HTML.safe_to_string()
    |> String.split("\n")
  end

  defp render_cell_status(%{evaluation_status: :evaluating}) do
    assigns = %{}

    ~L"""
    <div class="flex items-center space-x-2">
      <div class="text-xs text-gray-400">Evaluating</div>
      <span class="flex relative h-3 w-3">
        <span class="animate-ping absolute inline-flex h-3 w-3 rounded-full bg-blue-300 opacity-75"></span>
        <span class="relative inline-flex rounded-full h-3 w-3 bg-blue-400"></span>
      </span>
    </div>
    """
  end

  defp render_cell_status(%{evaluation_status: :queued}) do
    assigns = %{}

    ~L"""
    <div class="flex items-center space-x-2">
      <div class="text-xs text-gray-400">Queued</div>
      <span class="flex relative h-3 w-3">
        <span class="animate-ping absolute inline-flex h-3 w-3 rounded-full bg-gray-400 opacity-75"></span>
        <span class="relative inline-flex rounded-full h-3 w-3 bg-gray-500"></span>
      </span>
    </div>
    """
  end

  defp render_cell_status(%{validity_status: :evaluated}) do
    assigns = %{}

    ~L"""
    <div class="flex items-center space-x-2">
      <div class="text-xs text-gray-400">Evaluated</div>
      <div class="h-3 w-3 rounded-full bg-green-400"></div>
    </div>
    """
  end

  defp render_cell_status(%{validity_status: :stale}) do
    assigns = %{}

    ~L"""
    <div class="flex items-center space-x-2">
      <div class="text-xs text-gray-400">Stale</div>
      <div class="h-3 w-3 rounded-full bg-yellow-200"></div>
    </div>
    """
  end

  defp render_cell_status(_), do: nil
end

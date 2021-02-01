defmodule LiveBookWeb.Cell do
  use LiveBookWeb, :live_component

  def render(assigns) do
    ~L"""
    <div id="cell-<%= @cell.id %>"
         phx-hook="Cell"
         data-cell-id="<%= @cell.id %>"
         data-type="<%= @cell.type %>"
         data-focused="<%= @focused %>"
         data-expanded="<%= @expanded %>"
         class="flex flex-col relative mr-10 border-l-4 pl-4 -ml-4 border-blue-100 border-opacity-0 hover:border-opacity-100 <%= if @focused, do: "border-blue-300 border-opacity-100"%>">
      <%= render_cell_content(assigns) %>
    </div>
    """
  end

  def render_cell_content(%{cell: %{type: :markdown}} = assigns) do
    ~L"""
    <%= if @focused do %>
      <div class="flex flex-col items-center space-y-2 absolute right-0 top-0 -mr-10">
        <button phx-click="delete_cell" phx-value-cell_id="<%= @cell.id %>" class="text-gray-500 hover:text-current">
          <%= Icons.svg(:trash, class: "h-6") %>
        </button>
      </div>
    <% end %>

    <div class="<%= if @expanded, do: "mb-4", else: "hidden" %>">
      <%= render_editor(@cell) %>
    </div>

    <div class="markdown" data-markdown-container id="markdown-container-<%= @cell.id %>" phx-update="ignore">
      <%= render_markdown_content_placeholder(@cell.source) %>
    </div>
    """
  end

  def render_cell_content(%{cell: %{type: :elixir}} = assigns) do
    ~L"""
    <%= if @focused do %>
      <div class="flex flex-col items-center space-y-2 absolute right-0 top-0 -mr-10">
        <button phx-click="queue_cell_evaluation" phx-value-cell_id="<%= @cell.id %>" class="text-gray-500 hover:text-current">
          <%= Icons.svg(:play, class: "h-6") %>
        </button>
        <button phx-click="delete_cell" phx-value-cell_id="<%= @cell.id %>" class="text-gray-500 hover:text-current">
          <%= Icons.svg(:trash, class: "h-6") %>
        </button>
      </div>
    <% end %>

    <%= render_editor(@cell) %>

    <%= if length(@cell.outputs) > 0 do %>
      <div class="flex flex-col rounded-md mt-2 border border-gray-200 divide-y divide-gray-200 text-sm">
        <%= for output <- Enum.reverse(@cell.outputs) do %>
          <div class="p-4">
            <div class="max-h-80 overflow-auto tiny-scrollbar">
              <%= render_output(output) %>
            </div>
          </div>
        <% end %>
      </div>
    <% end %>
    """
  end

  defp render_editor(cell) do
    ~E"""
    <div class="py-3 rounded-md overflow-hidden bg-editor"
         data-editor-container
         id="editor-container-<%= cell.id %>"
         phx-update="ignore">
      <%= render_editor_content_placeholder(cell.source) %>
    </div>
    """
  end

  # The whole page has to load and then hooks are mounded.
  # There may be a tiny delay before the markdown is rendered
  # or and editors are mounted, so show neat placeholders immediately.

  defp render_markdown_content_placeholder("" = _content) do
    ~E"""
    <div class="h-4"></div>
    """
  end

  defp render_markdown_content_placeholder(_content) do
    ~E"""
    <div class="max-w-2xl w-full animate-pulse">
      <div class="flex-1 space-y-4">
        <div class="h-4 bg-gray-200 rounded w-3/4"></div>
        <div class="h-4 bg-gray-200 rounded"></div>
        <div class="h-4 bg-gray-200 rounded w-5/6"></div>
      </div>
    </div>
    """
  end

  defp render_editor_content_placeholder("" = _content) do
    ~E"""
    <div class="h-4"></div>
    """
  end

  defp render_editor_content_placeholder(_content) do
    ~E"""
    <div class="px-8 max-w-2xl w-full animate-pulse">
      <div class="flex-1 space-y-4 py-1">
        <div class="h-4 bg-gray-500 rounded w-3/4"></div>
        <div class="h-4 bg-gray-500 rounded"></div>
        <div class="h-4 bg-gray-500 rounded w-5/6"></div>
      </div>
    </div>
    """
  end

  defp render_output(output) when is_binary(output) do
    ~E"""
    <div class="whitespace-pre text-gray-500"><%= output %></div>
    """
  end

  defp render_output({:ok, value}) do
    ~E"""
    <div class="whitespace-pre text-gray-500"><%= inspect(value, pretty: true) %></div>
    """
  end

  defp render_output({:error, kind, error, stacktrace}) do
    ~E"""
    <div class="whitespace-pre text-red-600"><%= Exception.format(kind, error, stacktrace) %></div>
    """
  end
end

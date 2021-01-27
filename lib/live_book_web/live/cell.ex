defmodule LiveBookWeb.Cell do
  use LiveBookWeb, :live_component

  def render(%{cell: %{type: :markdown}} = assigns) do
    expanded = assigns.focused and assigns.focused_cell_expanded

    ~L"""
    <div id="cell-<%= @cell.id %>"
         data-cell-id="<%= @cell.id %>"
         class="flex flex-col relative mr-10 border-l-4 border-blue-100 border-opacity-0 hover:border-opacity-100 pl-4 -ml-4 <%= if @focused, do: "border-blue-300 border-opacity-100"%>">
      <div class="flex flex-col items-center space-y-2 absolute right-0 top-0 -mr-10">
        <button phx-click="delete_cell" phx-value-cell_id="<%= @cell.id %>" class="text-gray-500 hover:text-current">
          <%= Icons.svg(:trash, class: "h-6") %>
        </button>
      </div>

      <div class="<%= if expanded, do: "mb-4" %>">
        <%= render_editor(@cell, @cell_info, hidden: !expanded) %>
      </div>

      <%= if blank?(@cell.source) do %>
        <div class="text-gray-300">
          Empty markdown cell
        </div>
      <% else %>
        <%= render_markdown(@cell.source) %>
      <% end %>
    </div>
    """
  end

  def render(%{cell: %{type: :elixir}} = assigns) do
    ~L"""
    <div id="cell-<%= @cell.id %>"
         data-cell-id="<%= @cell.id %>"
         class="flex flex-col relative mr-10">
      <div class="flex flex-col items-center space-y-2 absolute right-0 top-0 -mr-10">
        <button class="text-gray-500 hover:text-current">
          <%= Icons.svg(:play, class: "h-6") %>
        </button>
        <button phx-click="delete_cell" phx-value-cell_id="<%= @cell.id %>" class="text-gray-500 hover:text-current">
          <%= Icons.svg(:trash, class: "h-6") %>
        </button>
      </div>
      <%= render_editor(@cell, @cell_info) %>
    </div>
    """
  end

  defp render_markdown(source) do
    {_, html, _} = Earmark.as_html(source)

    ~E"""
    <div class="markdown">
      <%= raw(html) %>
    </div>
    """
  end

  defp render_editor(cell, cell_info, opts \\ []) do
    hidden = Keyword.get(opts, :hidden, false)

    ~E"""
    <div
      class="py-3 rounded-md bg-editor <%= if hidden, do: "hidden" %>"
      id="cell-<%= cell.id %>-editor"
      phx-hook="Editor"
      phx-update="ignore"
      data-cell-id="<%= cell.id %>"
      data-type="<%= cell.type %>"
      data-hidden="<%= hidden %>">
      <div data-source="<%= cell.source %>"
           data-revision="<%= cell_info.revision %>">
        <%# The whole page has to load and then hooks are mounded.
            There's may be a tiny delay before the editor is being mounted,
            so show a nice placeholder immediately. %>
        <%= render_content_placeholder() %>
      </div>
    </div>
    """
  end

  defp render_content_placeholder() do
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

  defp blank?(string), do: String.trim(string) == ""
end

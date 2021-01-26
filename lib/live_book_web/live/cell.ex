defmodule LiveBookWeb.Cell do
  use LiveBookWeb, :live_component

  def render(%{cell: %{type: :markdown}} = assigns) do
    ~L"""
    <div phx-click="focus_cell"
         phx-value-cell_id="<%= @cell.id %>"
         class="flex flex-col relative mr-10 border-l-4 border-blue-100 border-opacity-0 hover:border-opacity-100 pl-4 -ml-4 <%= if @focused, do: "border-blue-300 border-opacity-100"%>">
      <div class="flex flex-col items-center space-y-2 absolute right-0 top-0 -mr-10">
        <button phx-click="delete_cell" phx-value-cell_id="<%= @cell.id %>" class="text-gray-500 hover:text-current">
          <%= Icons.svg(:trash, class: "h-6") %>
        </button>
      </div>

      <div class="<%= if @focused, do: "mb-4" %>">
        <%= render_editor(@cell, @cell_info, hidden: !@focused) %>
      </div>

      <%= render_markdown(@cell.source) %>
    </div>
    """
  end

  def render(%{cell: %{type: :elixir}} = assigns) do
    ~L"""
    <div phx-click="focus_cell"
         phx-value-cell_id="<%= @cell.id %>"
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
      </div>
    </div>
    """
  end
end

defmodule LivebookWeb.Output.TextComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~L"""
    <div id="virtualized-text-<%= @id %>"
      class="relative"
      phx-hook="VirtualizedLines"
      data-max-height="300"
      data-follow="<%= @follow %>">
      <div data-template class="hidden">
        <%= for line <- ansi_to_html_lines(@content) do %>
          <%# Add a newline, so that multiple lines can be copied properly %>
          <div><%= [line, "\n"] %></div>
        <% end %>
      </div>
      <div data-content class="overflow-auto whitespace-pre font-editor text-gray-500 tiny-scrollbar"
        id="virtualized-text-<%= @id %>-content"
        phx-update="ignore"></div>
      <div class="absolute right-4 top-0 z-10">
        <button class="icon-button bg-gray-100" data-clipboard>
          <%= remix_icon("clipboard-line", class: "text-lg") %>
        </button>
      </div>
    </div>
    """
  end
end

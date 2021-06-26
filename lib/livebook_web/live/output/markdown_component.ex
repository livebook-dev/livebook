defmodule LivebookWeb.Output.MarkdownComponent do
  use LivebookWeb, :live_component

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    {:ok,
     push_event(socket, "markdown-renderer:#{socket.assigns.id}:content", %{
       content: socket.assigns.content
     })}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="markdown"
      id="markdown-renderer-<%= @id %>"
      phx-hook="MarkdownRenderer"
      data-id="<%= @id %>">
    </div>
    """
  end
end

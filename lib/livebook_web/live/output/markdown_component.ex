defmodule LivebookWeb.Output.MarkdownComponent do
  use LivebookWeb, :live_component

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    {:ok,
     push_event(socket, "markdown_renderer:#{socket.assigns.id}:content", %{
       content: socket.assigns.content
     })}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="markdown"
      id={"markdown-renderer-#{@id}"}
      phx-hook="MarkdownRenderer"
      data-id={@id}
      data-session-path={Routes.session_path(@socket, :page, @session_id)}>
    </div>
    """
  end
end

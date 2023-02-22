defmodule LivebookWeb.Output.MarkdownComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, allowed_uri_schemes: Livebook.Config.allowed_uri_schemes())}
  end

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
    <div
      class="markdown"
      id={"markdown-renderer-#{@id}"}
      phx-hook="MarkdownRenderer"
      data-id={@id}
      data-session-path={~p"/sessions/#{@session_id}"}
      data-allowed-uri-schemes={Enum.join(@allowed_uri_schemes, ",")}
    >
    </div>
    """
  end
end

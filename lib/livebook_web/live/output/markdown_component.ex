defmodule LivebookWeb.Output.MarkdownComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, allowed_uri_schemes: Livebook.Config.allowed_uri_schemes(), chunks: 0),
     temporary_assigns: [text: nil]}
  end

  @impl true
  def update(assigns, socket) do
    {text, assigns} = Map.pop(assigns, :text)
    socket = assign(socket, assigns)

    if text do
      {:ok, socket |> assign(text: text) |> update(:chunks, &(&1 + 1))}
    else
      {:ok, socket}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id={"markdown-renderer-#{@id}"}
      phx-hook="MarkdownRenderer"
      data-base-path={~p"/sessions/#{@session_id}"}
      data-allowed-uri-schemes={Enum.join(@allowed_uri_schemes, ",")}
    >
      <div
        data-template
        id={"markdown-renderer-#{@id}-template"}
        class="text-gray-700 whitespace-pre-wrap hidden"
        phx-update="append"
        phx-no-format
      ><span :if={@text} id={"plain-text-#{@id}-chunk-#{@chunks}"}><%=
     @text  %></span></div>
      <div data-content class="markdown" id={"markdown-rendered-#{@id}-content"} phx-update="ignore">
      </div>
    </div>
    """
  end
end

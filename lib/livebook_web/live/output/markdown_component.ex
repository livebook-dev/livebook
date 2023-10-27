defmodule LivebookWeb.Output.MarkdownComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(allowed_uri_schemes: Livebook.Config.allowed_uri_schemes())
     |> stream(:chunks, [])}
  end

  @impl true
  def update(assigns, socket) do
    {text, assigns} = Map.pop(assigns, :text)

    socket = assign(socket, assigns)

    if text do
      chunk = %{id: Livebook.Utils.random_id(), text: text}
      {:ok, stream_insert(socket, :chunks, chunk)}
    else
      {:ok, socket}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id={@id}
      phx-hook="MarkdownRenderer"
      data-base-path={~p"/sessions/#{@session_id}"}
      data-allowed-uri-schemes={Enum.join(@allowed_uri_schemes, ",")}
    >
      <div
        data-template
        id={"#{@id}-template"}
        class="text-gray-700 whitespace-pre-wrap hidden"
        phx-update="stream"
        phx-no-format
      ><span :for={{dom_id, chunk}<- @streams.chunks} id={dom_id}><%= chunk.text %></span></div>
      <div data-content class="markdown" id={"#{@id}-content"} phx-update="ignore"></div>
    </div>
    """
  end
end

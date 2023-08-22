defmodule LivebookWeb.Output.PlainTextComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, chunks: 0), temporary_assigns: [text: nil]}
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
      id={"plain-text-#{@id}"}
      class="text-gray-700 whitespace-pre-wrap"
      phx-update="append"
      phx-no-format
    ><span :if={@text} id={"plain-text-#{@id}-chunk-#{@chunks}"}><%=
     @text  %></span></div>
    """
  end
end

defmodule LivebookWeb.CellStatusComponent do
  use LivebookWeb, :live_component

  def render(%{evaluation_status: :evaluating} = assigns) do
    render_status_indicator("Evaluating", "bg-blue-500", "bg-blue-400", assigns.changed)
  end

  def render(%{evaluation_status: :queued}) do
    render_status_indicator("Queued", "bg-gray-500", "bg-gray-400", false)
  end

  def render(%{validity_status: :evaluated} = assigns) do
    render_status_indicator("Evaluated", "bg-green-400", nil, assigns.changed)
  end

  def render(%{validity_status: :stale} = assigns) do
    render_status_indicator("Stale", "bg-yellow-200", nil, assigns.changed)
  end

  def render(assigns) do
    ~L"""
    <div></div>
    """
  end

  defp render_status_indicator(text, circle_class, animated_circle_class, show_changed) do
    assigns = %{
      text: text,
      circle_class: circle_class,
      animated_circle_class: animated_circle_class,
      show_changed: show_changed
    }

    ~L"""
    <div class="flex items-center space-x-1">
      <div class="flex text-xs text-gray-400">
        <%= @text %>
        <span class="<%= unless(@show_changed, do: "invisible") %>">*</span>
      </div>
      <span class="flex relative h-3 w-3">
        <span class="animate-ping absolute inline-flex h-3 w-3 rounded-full <%= @animated_circle_class %> opacity-75"></span>
        <span class="relative inline-flex rounded-full h-3 w-3 <%= @circle_class %>"></span>
      </span>
    </div>
    """
  end
end

defmodule LivebookWeb.VirtualizedLinesComponent do
  use LivebookWeb, :live_component

  # The component expects:
  #
  # * `id` - used as the element unique id
  # * `class` - additional CSS classes for the root element
  # * `lines` - a list of lines to render
  # * `max_height` - the container maximum height

  def render(assigns) do
    ~L"""
    <%= content_tag :div, "",
      class: "overflow-auto #{@class}",
      id: @id,
      phx_hook: "VirtualizedLines",
      phx_update: "ignore",
      data_lines: Jason.encode!(@lines),
      data_max_height: @max_height %>
    """
  end
end

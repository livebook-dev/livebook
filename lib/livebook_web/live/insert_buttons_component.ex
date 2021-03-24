defmodule LivebookWeb.InsertButtonsComponent do
  use LivebookWeb, :live_component

  def render(assigns) do
    ~L"""
    <div class="relative top-0.5 m-0 flex justify-center">
      <div class="absolute z-10 <%= if(@persistent, do: "opacity-100", else: "opacity-0") %> hover:opacity-100 focus-within:opacity-100 flex space-x-2 justify-center items-center">
        <%= for button <- @buttons do %>
          <%= content_tag :button, button.label,
                Keyword.merge(
                  button.attrs,
                  class: "py-1 px-2 text-sm text-gray-600 font-medium rounded-lg border border-gray-200 bg-gray-50 hover:bg-gray-100 focus:bg-gray-100"
                ) %>
        <% end %>
      </div>
    </div>
    """
  end
end

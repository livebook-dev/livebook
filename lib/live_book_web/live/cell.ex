defmodule LiveBookWeb.Cell do
  use LiveBookWeb, :live_component

  def render(assigns) do
    ~L"""
    <div class="<%= cell_class(@focused) %>" phx-click="focus_cell" phx-value-cell_id="<%= @cell.id %>">
      <div class="flex flex-col items-center space-y-2 absolute right-0 top-0 -mr-10">
        <button class="text-gray-500 hover:text-current">
          <%= Icons.svg(:play, class: "h-6") %>
        </button>
        <button phx-click="delete_cell" phx-value-cell_id="<%= @cell.id %>" class="text-gray-500 hover:text-current">
          <%= Icons.svg(:trash, class: "h-6") %>
        </button>
        <%= cell_type_icon(@cell.type) %>
      </div>
      <div
        id="cell-<%= @cell.id %>"
        phx-hook="Editor"
        phx-update="ignore"
        data-id="<%= @cell.id %>"
        data-type="<%= @cell.type %>"
      >
        <div class="h-20 flex opacity-20">Editor placeholder</div>
      </div>
      <%= for output <- @cell.outputs do %>
        <div class="p-2">
          <%= render_output(output) %>
        </div>
      <% end %>
    </div>
    """
  end

  defp cell_class(focused) do
    base = "flex flex-col p-2 border-2 border-gray-200 rounded border-opacity-0 relative mr-10"
    if focused, do: base <> " border-opacity-100", else: base
  end

  defp render_output(_), do: nil

  defp cell_type_icon(:elixir) do
    ~e"""
    <svg xmlns="http://www.w3.org/2000/svg" width="32" height="32"><path fill="#8251A8" d="M16.457 27.304c-4.118 0-7.457-3.971-7.457-8.87 0-4.012 2.96-8.915 5.244-11.901C15.326 5.119 17.371 4 17.371 4s-1.048 5.714 1.794 7.984c2.523 2.014 4.38 4.635 4.38 6.94 0 4.618-2.97 8.38-7.088 8.38z"/></svg>
    """
  end

  defp cell_type_icon(:markdown) do
    ~e"""
    <svg xmlns="http://www.w3.org/2000/svg" width="32" height="32"><path fill="#EE9D5C" fill-rule="evenodd" d="M17.74 21.419h-3.48v-5.416l-2.631 3.463-2.631-3.463v5.416h-3.48V10.586h3.48l2.63 3.625 2.632-3.625h3.48V21.42zm5.223.874L18.63 16h2.631v-5.416h3.48V16h2.632l-4.409 6.293h-.002zM27.99 7H4.01c-.541 0-1.012.206-1.411.617-.4.411-.599.895-.599 1.453v13.86c0 .585.2 1.076.599 1.474.4.397.87.596 1.41.596H27.99c.541 0 1.012-.199 1.411-.596.4-.398.599-.89.599-1.474V9.07c0-.558-.2-1.042-.599-1.453-.4-.411-.87-.617-1.41-.617z"/></svg>
    """
  end
end

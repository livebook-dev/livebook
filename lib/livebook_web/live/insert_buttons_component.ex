defmodule LivebookWeb.InsertButtonsComponent do
  use LivebookWeb, :live_component

  def render(assigns) do
    ~L"""
    <div class="relative top-0.5 m-0 flex justify-center">
      <div class="absolute z-10 <%= if(@persistent, do: "opacity-100", else: "opacity-0") %> hover:opacity-100 focus-within:opacity-100 flex space-x-2 justify-center items-center">
        <%= render_block(@inner_block) %>
      </div>
    </div>
    """
  end
end

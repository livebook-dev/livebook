defmodule LivebookWeb.SessionLive.InsertButtonsComponent do
  use LivebookWeb, :live_component

  def render(assigns) do
    ~H"""
    <div class="relative top-0.5 m-0 flex justify-center"
      role="toolbar"
      aria-label="insert new"
      data-element="insert-buttons">
      <div class={"w-full absolute z-10 #{if(@persistent, do: "opacity-100", else: "opacity-0")} hover:opacity-100 focus-within:opacity-100 flex space-x-2 justify-center items-center"}>
        <button class="button-base button-small"
          phx-click="insert_cell_below"
          phx-value-type="markdown"
          phx-value-section_id={@section_id}
          phx-value-cell_id={@cell_id}
          >+ Markdown</button>
        <button class="button-base button-small"
          phx-click="insert_cell_below"
          phx-value-type="elixir"
          phx-value-section_id={@section_id}
          phx-value-cell_id={@cell_id}
          >+ Elixir</button>
        <button class="button-base button-small"
          phx-click="insert_section_below"
          phx-value-section_id={@section_id}
          phx-value-cell_id={@cell_id}
          >+ Section</button>
      </div>
    </div>
    """
  end
end

defmodule LivebookWeb.TeamsComponents do
  use Phoenix.Component

  alias LivebookWeb.LayoutHelpers

  @doc """
  Renders the teams header with the badges.
  """
  attr :hub, Livebook.Hubs.Team, required: true
  attr :hub_metadata, Livebook.Hubs.Metadata, required: true
  attr :default?, :boolean, required: true

  def header(assigns) do
    ~H"""
    <LayoutHelpers.title>
      <div class="flex gap-2 items-center">
        <div class="flex justify-center">
          <span class="relative">
            <%= @hub.hub_emoji %>
            <div class={[
              "absolute w-[10px] h-[10px] border-white border-2 rounded-full right-0 bottom-1",
              if(@hub_metadata.connected?, do: "bg-green-400", else: "bg-red-400")
            ]} />
          </span>
        </div>
        <%= @hub.hub_name %>
        <span class="bg-green-100 text-green-800 text-xs px-2.5 py-0.5 rounded cursor-default">
          Livebook Teams
        </span>
        <%= if @default? do %>
          <span class="bg-blue-100 text-blue-800 text-xs px-2.5 py-0.5 rounded cursor-default">
            Default
          </span>
        <% end %>
      </div>
    </LayoutHelpers.title>
    """
  end
end

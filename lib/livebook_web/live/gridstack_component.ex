defmodule LivebookWeb.GridstackComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  # The component expects:
  #
  #   * `:output_blocks` - TODO

  @impl true
  def update(%{app_settings: app_settings, output_blocks: output_blocks} = assigns, socket) do
    socket =
      socket
      |> assign(
        session: assigns.session,
        output_blocks: restore_layout(app_settings.output_layout, output_blocks),
        app_settings: app_settings
      )

    {:ok, socket}
  end

  defp restore_layout(output_layout, output_blocks) do
    restored_layout =
      Enum.into(output_layout, %{}, fn map ->
        {id, rest} = Map.pop(map, "id")
        {id, rest}
      end)

    Map.merge(output_blocks, restored_layout)
  end

  @impl true
  def render(assigns) do
    # TODO remove phx-update="ignore". Currently gridstack crashes without it
    ~H"""
    <div id="gridstack-container" phx-update="ignore">
      <div
        id="gridstack-grid"
        class="grid-stack"
        gs-column="12"
        phx-hook="Gridstack"
        data-phx-target={@myself}
      >
        <div
          :for={{id, params} <- @output_blocks}
          class="relative grid-stack-item rounded border-2 border-red-500"
          gs-id={id}
          gs-x={params["x"]}
          gs-y={params["y"]}
          gs-w={params["w"]}
          gs-h={params["h"]}
        >
          <div class="absolute grid-stack-item-content">
            <div class="absolute inset-0 bg-transparent z-20 hover:cursor-move" />
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("items_changed", params, socket) do
    updated = Map.merge(socket.assigns.output_blocks, params)

    output_layout =
      for {id, rest} <- updated do
        Map.put(rest, "id", id)
      end

    app_settings = %{socket.assigns.app_settings | output_layout: output_layout}

    Session.set_app_settings(
      socket.assigns.session.pid,
      app_settings
    )

    {:noreply, assign(socket, output_blocks: updated, app_settings: app_settings)}
  end
end

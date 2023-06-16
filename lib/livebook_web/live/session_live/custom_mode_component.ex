defmodule LivebookWeb.SessionLive.CustomModeComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    socket =
      assign(socket,
        section: false
      )

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-5">
      <h4 class="text-2xl font-semibold text-gray-800">
        Custom View
      </h4>
      <div class="flex flex-col space-y-4">
            <h6 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
              Select display options
            </h6>
            <div
              class="flex flex-col space-y-3"
              id="custom-settings"
            >
              <.switch_field
                name="custom_section"
                label="Show sections"
                value={false}
              />
              <.switch_field
                name="custom_markdown"
                label="Show markdown"
                value={false}
              />
              <.switch_field name="custom_results"
              label="Show results"
               value={false} />
              <.switch_field
              name="custom_output"
               label="Show outputs" value={false} />
              <.switch_field
                data-el-view-toggle
                name="custom_spotlight"
                label="Spotlight focused"
                value={false}
              />
            </div>
          </div>

          <div class="mt-8 flex justify-end space-x-2">
          <.link patch={@return_to} class="button-base button-outlined-gray">
            Cancel
          </.link>
          <button
          phx-click={
             JS.push("view", target: @myself)
            }
           class="button-base button-blue">
            Continue
          </button>
        </div>


    </div>
    """
  end

  @impl true
  def handle_event("view", %{}, socket) do
    {:noreply, socket
      |> push_patch(attrs: %{"data-el-view-toggle" => "presentation"}, to: socket.assigns.return_to, replace: true)
    }
  end
end

defmodule LivebookWeb.SessionLive.CustomViewComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6">
      <h3 class="text-2xl font-semibold text-gray-800">
        Custom view
      </h3>
      <p class="mt-8 text-gray-700">
        Configure notebook display options.
      </p>
      <h3 class="mt-8 text-lg font-semibold text-gray-800">
        Options
      </h3>
      <div
        class="mt-2 flex flex-col gap-2"
        id="custom-settings"
        data-el-custom
        phx-hook="CustomViewSettings"
        phx-update="ignore"
      >
        <.switch_field name="show_section" label="Show sections" value={false} />
        <.switch_field name="show_markdown" label="Show markdown" value={false} />
        <.switch_field name="show_output" label="Show outputs" value={false} />
        <.switch_field name="spotlight" label="Spotlight focused" value={false} />
      </div>
      <div class="mt-8 flex justify-end space-x-2">
        <.link patch={@return_to} class="button-base button-outlined-gray">
          Close
        </.link>
      </div>
    </div>
    """
  end
end

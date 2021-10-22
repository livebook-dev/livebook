defmodule LivebookWeb.SettingsLive.RuntimeSettingsComponent do
  use LivebookWeb, :live_component

  # The component expects:
  #
  #   * `evaluates_automatically?` - the option that indicates if automatic evaluation of cell is active.
  #
  # The parent live view receives a `:trigger_automatic_evaluation`
  # message whenever the user toggles the automatic evaluation option.

  @impl true
  def mount(socket) do
    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="h-full flex flex-col">
      <div class="flex space-x-3 items-center mb-4">
      <div>
        Evaluate Automatically:
      </div>

      <div>
        <.toggle_display automatic_evaluation_is_on={@evaluates_automatically?} />
      </div>

      </div>
    </div>
    """
  end

  defp toggle_display(assigns) do
    {container_class, toggle_class} =
      if assigns.automatic_evaluation_is_on,
        do:
          {"w-14 h-7 flex items-center bg-blue-700 rounded-full mx-3 px-1",
           "bg-white w-5 h-5 rounded-full shadow-md transform translate-x-7"},
        else:
          {"w-14 h-7 flex items-center bg-gray-300 rounded-full mx-3 px-1",
           "bg-white w-5 h-5 rounded-full shadow-md transform"}

    ~H"""
         <div
            phx-click="trigger_automatic_evaluation"
            class={container_class}
          >
            <div class={toggle_class} />
          </div>
    """
  end
end

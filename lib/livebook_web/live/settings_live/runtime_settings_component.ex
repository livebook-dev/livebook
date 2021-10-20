defmodule LivebookWeb.SettingsLive.RuntimeSettingsComponent do
  use LivebookWeb, :live_component

  # The component expects:
  #
  #   * `file` - the currently entered file
  #
  #   * `running_files` - the list of notebook files that are already
  #     linked to running sessions
  #
  #   * `extnames` - a list of file extensions that should be shown
  #
  #   * `submit_event` - the process event sent on form submission,
  #     use `nil` for no action
  #
  # The parent live view receives a `{:set_file, file, %{exists: boolean()}}`
  # message whenever the file changes.
  #
  # Optionally inner block may be passed (e.g. with action buttons)
  # and it's rendered next to the text input.
  #
  # To force the component to refetch the displayed files you can
  # `send_update` with `force_reload: true` to the component.

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
        <.toggle_display status={@evaluates_automatically?} />
      </div>

      </div>
    </div>
    """
  end


  defp toggle_display(assigns) do
    {container_class, toggle_class} =
      if assigns.status,
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

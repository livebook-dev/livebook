defmodule LivebookWeb.SettingsLive.AutosavePathComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <div class="flex items-center justify-between border border-gray-200 rounded-lg p-4">
        <div class="h-full h-52">
          <.live_component module={LivebookWeb.FileSelectComponent}
              id="autosave-path-component"
              file={@autosave_file}
              extnames={[]}
              running_files={[]}
              submit_event={:init}
              file_system_select_disabled={true} />
        </div>
      </div>
      <div class="flex">
        <button
          class="button-base button-blue mt-4 mr-4"
          phx-click="set_autosave_path"
        >
          Save
        </button>
        <button
          class="button-base button-red mt-4"
          phx-click="reset_autosave_path"
        >
          Reset
        </button>
      </div>
    </div>
    """
  end
end

defmodule LivebookWeb.SettingsLive.AutosavePathComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex items-center justify-between border border-gray-200 rounded-lg p-4">
      <div class={"w-full #{if @state.open_file_select, do: "h-52", else: "h-8"}"}>
        <%= if @state.open_file_select do %>
          <.live_component module={LivebookWeb.FileSelectComponent}
            id="autosave-path-component"
            file={@state.file}
            extnames={[]}
            running_files={[]}
            submit_event={:init}
            file_system_select_disabled={true}
          >
            <button class="button-base button-gray"
                phx-click="cancel_autosave_path"
                tabindex="-1">
                Cancel
            </button>
            <button class="button-base button-red"
                phx-click="reset_autosave_path"
                tabindex="-1">
                Reset
            </button>
            <button class="button-base button-blue"
              phx-click="set_autosave_path"
              disabled={not Livebook.FileSystem.File.dir?(@state.file)}
              tabindex="-1">
              Choose
            </button>
          </.live_component>
        <% else %>
          <div class="flex">
            <input class="input mr-2" readonly value={@state.file.path}/>
            <button class="button-base button-gray button-small"
              phx-click="switch_autosave_path_select">
              Choose a path
            </button>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  def default_state() do
    %{
      file: current_autosave_file(),
      open_file_select: false
    }
  end

  def switch_file_select(state) do
    %{state | open_file_select: not state.open_file_select}
  end

  def reload_file(state) do
    %{state | file: current_autosave_file()}
  end

  def update_file(state, file) do
    %{state | file: file}
  end

  defp current_autosave_file() do
    Livebook.Settings.autosave_path()
    |> Livebook.FileSystem.Utils.ensure_dir_path()
    |> Livebook.FileSystem.File.local()
  end
end

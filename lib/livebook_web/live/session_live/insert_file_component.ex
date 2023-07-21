defmodule LivebookWeb.SessionLive.InsertFileComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-3">
      <h3 class="text-2xl font-semibold text-gray-800">
        Insert file
      </h3>
      <p class="text-gray-700">
        What do you want to do with the file?
      </p>
      <div class="w-full flex flex-col space-y-4">
        <div
          :for={{handler, idx} <- Enum.with_index(@insert_file_metadata.handlers)}
          class="px-4 py-3 border border-gray-200 rounded-xl text-gray-800 pointer hover:bg-gray-50 cursor-pointer"
          phx-click={JS.push("insert_file_action", value: %{idx: idx})}
        >
          <span>
            <%= handler.definition.description %>
          </span>
        </div>
      </div>
    </div>
    """
  end
end

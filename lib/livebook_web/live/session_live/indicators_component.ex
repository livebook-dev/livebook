defmodule LivebookWeb.SessionLive.IndicatorsComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col items-center space-y-2" data-element="notebook-indicators">
      <%= if @file do %>
        <%= if @dirty do %>
          <%= if @autosave_interval_s do %>
            <span class="tooltip left" aria-label="Autosave pending">
              <%= live_patch to: Routes.session_path(@socket, :file_settings, @session_id),
                    class: "icon-button icon-outlined-button border-blue-400 hover:bg-blue-50 focus:bg-blue-50" do %>
                <.remix_icon icon="save-line" class="text-xl text-blue-500" />
              <% end %>
            </span>
          <% else %>
            <span class="tooltip left" aria-label="No autosave configured, make sure to save manually">
              <%= live_patch to: Routes.session_path(@socket, :file_settings, @session_id),
                    class: "icon-button icon-outlined-button border-yellow-200 hover:bg-red-50 focus:bg-red-50" do %>
                <.remix_icon icon="save-line" class="text-xl text-yellow-300" />
              <% end %>
            </span>
          <% end %>
        <% else %>
          <span class="tooltip left" aria-label="Notebook saved">
            <%= live_patch to: Routes.session_path(@socket, :file_settings, @session_id),
                  class: "icon-button icon-outlined-button border-green-300 hover:bg-green-50 focus:bg-green-50" do %>
              <.remix_icon icon="save-line" class="text-xl text-green-400" />
            <% end %>
          </span>
        <% end %>
      <% else %>
        <span class="tooltip left" aria-label="Choose a file to save the notebook">
          <%= live_patch to: Routes.session_path(@socket, :file_settings, @session_id),
                class: "icon-button icon-outlined-button border-gray-200 hover:bg-gray-100 focus:bg-gray-100" do %>
            <.remix_icon icon="save-line" class="text-xl text-gray-400" />
          <% end %>
        </span>
      <% end %>

      <%= if @runtime do %>
        <.global_status
          status={elem(@global_status, 0)}
          cell_id={elem(@global_status, 1)} />
      <% else %>
        <span class="tooltip left" aria-label="Choose a runtime to run the notebook in">
          <%= live_patch to: Routes.session_path(@socket, :runtime_settings, @session_id),
                class: "icon-button icon-outlined-button border-gray-200 hover:bg-gray-100 focus:bg-gray-100" do %>
            <.remix_icon icon="loader-3-line" class="text-xl text-gray-400" />
          <% end %>
        </span>
      <% end %>

      <%# Note: this indicator is shown/hidden using CSS based on the current mode %>
      <span class="tooltip left" aria-label="Insert mode" data-element="insert-mode-indicator">
        <span class="text-sm font-medium text-gray-400 cursor-default">
          ins
        </span>
      </span>
    </div>
    """
  end

  defp global_status(%{status: :evaluating} = assigns) do
    ~H"""
    <span class="tooltip left" aria-label="Go to evaluating cell">
      <button class="border-blue-400 icon-button icon-outlined-button hover:bg-blue-50 focus:bg-blue-50"
        data-element="focus-cell-button"
        data-target={@cell_id}>
        <.remix_icon icon="loader-3-line" class="text-xl text-blue-500 animate-spin" />
      </button>
    </span>
    """
  end

  defp global_status(%{status: :evaluated} = assigns) do
    ~H"""
    <span class="tooltip left" aria-label="Go to last evaluated cell">
      <button class="border-green-300 icon-button icon-outlined-button hover:bg-green-50 focus:bg-green-50"
        data-element="focus-cell-button"
        data-target={@cell_id}>
        <.remix_icon icon="loader-3-line" class="text-xl text-green-400" />
      </button>
    </span>
    """
  end

  defp global_status(%{status: :stale} = assigns) do
    ~H"""
    <span class="tooltip left" aria-label="Go to first stale cell">
      <button class="border-yellow-200 icon-button icon-outlined-button hover:bg-yellow-50 focus:bg-yellow-50"
        data-element="focus-cell-button"
        data-target={@cell_id}>
        <.remix_icon icon="loader-3-line" class="text-xl text-yellow-300" />
      </button>
    </span>
    """
  end

  defp global_status(%{status: :fresh} = assigns) do
    ~H"""
    <span class="tooltip left" aria-label="Ready to evaluate">
      <button class="border-gray-200 cursor-default icon-button icon-outlined-button hover:bg-gray-100 focus:bg-gray-100">
        <.remix_icon icon="loader-3-line" class="text-xl text-gray-400" />
      </button>
    </span>
    """
  end
end

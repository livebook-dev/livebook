defmodule LivebookWeb.SessionLive.IndicatorsComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-2 items-center" data-element="notebook-indicators">
      <%= if @path do %>
        <%= if @dirty do %>
          <span class="tooltip left" aria-label="Autosave pending">
            <%= live_patch to: Routes.session_path(@socket, :file_settings, @session_id),
                  class: "icon-button icon-outlined-button border-blue-400 hover:bg-blue-50 focus:bg-blue-50" do %>
              <.remix_icon icon="save-line" class="text-xl text-blue-500" />
            <% end %>
          </span>
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
        <.global_evaluation_status
          status={elem(@global_evaluation_status, 0)}
          cell_id={elem(@global_evaluation_status, 1)} />
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
        <span class="text-sm text-gray-400 font-medium cursor-default">
          ins
        </span>
      </span>
    </div>
    """
  end

  defp global_evaluation_status(%{status: :evaluating} = assigns) do
    ~H"""
    <span class="tooltip left" aria-label="Go to evaluating cell">
      <button class="icon-button icon-outlined-button border-blue-400 hover:bg-blue-50 focus:bg-blue-50"
        data-element="focus-cell-button"
        data-target={@cell_id}>
        <.remix_icon icon="loader-3-line" class="text-xl text-blue-500 animate-spin" />
      </button>
    </span>
    """
  end

  defp global_evaluation_status(%{status: :evaluated} = assigns) do
    ~H"""
    <span class="tooltip left" aria-label="Go to last evaluated cell">
      <button class="icon-button icon-outlined-button border-green-300 hover:bg-green-50 focus:bg-green-50"
        data-element="focus-cell-button"
        data-target={@cell_id}>
        <.remix_icon icon="loader-3-line" class="text-xl text-green-400" />
      </button>
    </span>
    """
  end

  defp global_evaluation_status(%{status: :stale} = assigns) do
    ~H"""
    <span class="tooltip left" aria-label="Go to first stale cell">
      <button class="icon-button icon-outlined-button border-yellow-200 hover:bg-yellow-50 focus:bg-yellow-50"
        data-element="focus-cell-button"
        data-target={@cell_id}>
        <.remix_icon icon="loader-3-line" class="text-xl text-yellow-300" />
      </button>
    </span>
    """
  end

  defp global_evaluation_status(%{status: :fresh} = assigns) do
    ~H"""
    <span class="tooltip left" aria-label="Ready to evaluate">
      <button class="icon-button icon-outlined-button border-gray-200 hover:bg-gray-100 focus:bg-gray-100 cursor-default">
        <.remix_icon icon="loader-3-line" class="text-xl text-gray-400" />
      </button>
    </span>
    """
  end
end

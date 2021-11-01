defmodule LivebookWeb.WithPasswordToggleComponent do
  use LivebookWeb, :live_component

  @impl Phoenix.LiveComponent
  def render(assigns) do
    ~H"""
    <div id={"password-toggle-#{@id}"} class="flex input w-min items-center" phx-hook="PasswordToggle">
      <!-- render password input -->
      <%= render_block(@inner_block) %>
      <button type="button" phx-change="ignore" class="p-1 icon-button right-0"aria-label="toggle-password-visibility">
        <.remix_icon icon="eye-line" class="text-xl" />
      </button>
    </div>
    """
  end
end

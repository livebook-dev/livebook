defmodule LivebookWeb.ModalComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="fixed z-[10000] inset-0 fade-in"
      phx-remove={JS.transition("fade-out")}>
      <!-- Modal container -->
      <div class="h-screen flex items-center justify-center p-4">
        <!-- Overlay -->
        <div class="absolute inset-0 bg-gray-500 opacity-75 z-0" aria-hidden="true"></div>

        <!-- Modal box -->
        <div class={"relative max-h-full overflow-y-auto bg-white rounded-lg shadow-xl #{@modal_class}"}
          role="dialog"
          aria-modal="true"
          phx-window-keydown={click_modal_close()}
          phx-click-away={click_modal_close()}
          phx-key="escape">

          <%= live_patch to: @return_to,
                class: "absolute top-6 right-6 text-gray-400 flex space-x-1 items-center",
                aria_label: "close modal",
                id: "close-modal-button" do %>
            <span class="text-sm">(esc)</span>
            <.remix_icon icon="close-line" class="text-2xl" />
          <% end %>

          <%=
            case @render_spec do
              {:component, component, opts} -> live_component(component, opts)
              {:live_view, socket, live_view, opts} -> live_render(socket, live_view, opts)
            end
          %>
        </div>
      </div>
    </div>
    """
  end

  defp click_modal_close(js \\ %JS{}) do
    JS.dispatch(js, "click", to: "#close-modal-button")
  end
end

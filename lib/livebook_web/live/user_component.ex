defmodule LivebookWeb.UserComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.LiveHelpers

  alias Livebook.Users.User

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)
    user = socket.assigns.user

    data = %{"name" => user.name || "", "color" => user.color}
    {:ok, assign(socket, data: data, preview_user: user)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="p-6 flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        User profile
      </h3>
      <div class="flex justify-center">
        <%= render_user_avatar(@preview_user.name, @preview_user.color, class: "h-20 w-20", text_class: "text-3xl") %>
      </div>
      <%= f = form_for :data, "#",
                id: "user_form",
                phx_target: @myself,
                phx_submit: "save",
                phx_change: "validate",
                phx_hook: "UserForm" %>
        <div class="flex flex-col space-y-5">
          <div>
            <div class="input-label">Display name</div>
            <%= text_input f, :name, value: @data["name"], class: "input", spellcheck: "false" %>
          </div>
          <div>
            <div class="input-label">Cursor color</div>
            <div class="flex space-x-4 items-center">
              <div class="border-[3px] rounded-lg p-1 flex justify-center items-center"
                style="border-color: <%= @preview_user.color %>">
                <div class="rounded h-5 w-5"
                  style="background-color: <%= @preview_user.color %>">
                </div>
              </div>
              <div class="relative flex-grow">
                <%= text_input f, :color, value: @data["color"], class: "input", spellcheck: "false", maxlength: 7 %>
                <%= tag :button, class: "icon-button absolute right-2 top-1",
                      type: "button",
                      phx_click: "randomize_color",
                      phx_target: @myself %>
                  <%= remix_icon("refresh-line", class: "text-xl") %>
                </button>
              </div>
            </div>
          </div>
          <%= tag :button, class: "button button-blue flex space-x-1 justify-center items-center",
                type: "submit",
                disabled: not data_valid?(@data) %>
            <%= remix_icon("save-line") %>
            <span>Save</span>
          </button>
        </div>
      </form>
    </div>
    """
  end

  @impl true
  def handle_event("randomize_color", %{}, socket) do
    data = %{socket.assigns.data | "color" => User.random_color()}
    {:noreply, assign(socket, data: data, preview_user: data_to_preview_user(data))}
  end

  def handle_event("validate", %{"data" => data}, socket) do
    {:noreply, assign(socket, data: data, preview_user: data_to_preview_user(data))}
  end

  def handle_event("save", %{"data" => data}, socket) do
    preview_user = data_to_preview_user(data)
    user = %{preview_user | id: socket.assigns.user.id}
    Livebook.Users.update(user)
    {:noreply, push_patch(socket, to: socket.assigns.return_to)}
  end

  defp data_valid?(data) do
    User.color_valid?(data["color"])
  end

  defp data_to_preview_user(data) do
    User.new(%{
      name:
        case data["name"] do
          "" -> nil
          name -> name
        end,
      color: if(User.color_valid?(data["color"]), do: data["color"], else: "#304254")
    })
  end
end

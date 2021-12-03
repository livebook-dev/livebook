defmodule LivebookWeb.UserComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.UserHelpers

  alias Livebook.Users.User

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)
    user = socket.assigns.user

    {:ok, assign(socket, data: user_to_data(user), valid: true, preview_user: user)}
  end

  defp user_to_data(user) do
    %{"name" => user.name || "", "hex_color" => user.hex_color}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        User profile
      </h3>
      <div class="flex justify-center">
        <.user_avatar user={@preview_user} class="h-20 w-20" text_class="text-3xl" />
      </div>
      <.form let={f} for={:data}
        phx-submit="save"
        phx-change="validate"
        phx-target={@myself}
        id="user_form"
        phx-hook="UserForm">
        <div class="flex flex-col space-y-5">
          <div>
            <div class="input-label">Display name</div>
            <%= text_input f, :name, value: @data["name"], class: "input", spellcheck: "false" %>
          </div>
          <div>
            <div class="input-label">Cursor color</div>
            <div class="flex space-x-4 items-center">
              <div class="border-[3px] rounded-lg p-1 flex justify-center items-center"
                style={"border-color: #{@preview_user.hex_color}"}>
                <div class="rounded h-5 w-5"
                  style={"background-color: #{@preview_user.hex_color}"}>
                </div>
              </div>
              <div class="relative flex-grow">
                <%= text_input f, :hex_color, value: @data["hex_color"], class: "input", spellcheck: "false", maxlength: 7 %>
                <button
                  class="icon-button absolute right-2 top-1"
                  type="button"
                  phx-click="randomize_color"
                  phx-target={@myself}>
                  <.remix_icon icon="refresh-line" class="text-xl" />
                </button>
              </div>
            </div>
          </div>
          <button
            class="button-base button-blue flex space-x-1 justify-center items-center"
            type="submit"
            disabled={not @valid}>
            <.remix_icon icon="save-line" />
            <span>Save</span>
          </button>
        </div>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("randomize_color", %{}, socket) do
    data = %{
      socket.assigns.data
      | "hex_color" => User.random_hex_color(except: [socket.assigns.preview_user.hex_color])
    }

    handle_event("validate", %{"data" => data}, socket)
  end

  def handle_event("validate", %{"data" => data}, socket) do
    {valid, user} =
      case User.change(socket.assigns.user, data) do
        {:ok, user} -> {true, user}
        {:error, _errors, user} -> {false, user}
      end

    {:noreply, assign(socket, data: data, valid: valid, preview_user: user)}
  end

  def handle_event("save", %{"data" => data}, socket) do
    {:ok, user} = User.change(socket.assigns.user, data)
    Livebook.Users.broadcast_change(user)
    {:noreply, push_patch(socket, to: socket.assigns.return_to)}
  end
end

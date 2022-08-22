defmodule LivebookWeb.UserComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.UserHelpers

  alias Livebook.EctoTypes.HexColor
  alias Livebook.Users

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)
    user = socket.assigns.user
    changeset = Users.change_user(user)

    {:ok, assign(socket, changeset: changeset, valid?: changeset.valid?, user: user)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        User profile
      </h3>
      <div class="flex justify-center">
        <.user_avatar user={@user} class="h-20 w-20" text_class="text-3xl" />
      </div>
      <.form
        let={f}
        for={@changeset}
        phx-submit={@on_save |> JS.push("save")}
        phx-change="validate"
        phx-target={@myself}
        id="user_form"
        phx-hook="UserForm"
      >
        <div class="flex flex-col space-y-5">
          <div>
            <div class="input-label">Display name</div>
            <%= text_input(f, :name, class: "input", spellcheck: "false") %>
            <%= error_tag(f, :name) %>
          </div>
          <div>
            <div class="input-label">Cursor color</div>
            <div class="flex space-x-4 items-center">
              <div
                class="border-[3px] rounded-lg p-1 flex justify-center items-center"
                style={"border-color: #{hex_color(@changeset)}"}
              >
                <div class="rounded h-5 w-5" style={"background-color: #{hex_color(@changeset)}"}>
                </div>
              </div>
              <div class="relative grow">
                <%= text_input(f, :hex_color,
                  class: "input",
                  spellcheck: "false",
                  maxlength: 7
                ) %>
                <button
                  class="icon-button absolute right-2 top-1"
                  type="button"
                  phx-click="randomize_color"
                  phx-target={@myself}
                >
                  <.remix_icon icon="refresh-line" class="text-xl" />
                </button>
                <%= error_tag(f, :hex_color) %>
              </div>
            </div>
          </div>
          <button
            class="button-base button-blue flex space-x-1 justify-center items-center"
            type="submit"
            disabled={not @valid?}
          >
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
    hex_color = HexColor.random(except: [socket.assigns.user.hex_color])
    handle_event("validate", %{"user" => %{"hex_color" => hex_color}}, socket)
  end

  def handle_event("validate", %{"user" => params}, socket) do
    changeset = Users.change_user(socket.assigns.user, params)

    user =
      if changeset.valid? do
        Ecto.Changeset.apply_action!(changeset, :update)
      else
        socket.assigns.user
      end

    {:noreply, assign(socket, changeset: changeset, valid?: changeset.valid?, user: user)}
  end

  def handle_event("save", %{"user" => params}, socket) do
    changeset = Users.change_user(socket.assigns.user, params)

    case Users.update_user(changeset) do
      {:ok, user} ->
        {:noreply, assign(socket, changeset: changeset, valid?: changeset.valid?, user: user)}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset, valid?: changeset.valid?)}
    end
  end

  defp hex_color(changeset), do: Ecto.Changeset.get_field(changeset, :hex_color)
end

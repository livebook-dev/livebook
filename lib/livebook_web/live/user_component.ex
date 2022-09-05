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
          <.input_wrapper form={f} field={:name}>
            <div class="input-label">Display name</div>
            <%= text_input(f, :name, class: "input phx-form-error:border-red-300", spellcheck: "false") %>
          </.input_wrapper>
          <.input_wrapper form={f} field={:hex_color}>
            <div class="input-label">Cursor color</div>
            <.hex_color_input
              form={f}
              field={:hex_color}
              phx-click="randomize_color"
              phx-target={@myself}
            />
          </.input_wrapper>
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
end

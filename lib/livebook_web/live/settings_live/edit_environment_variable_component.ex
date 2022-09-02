defmodule LivebookWeb.SettingsLive.EditEnvironmentVariableComponent do
  use LivebookWeb, :live_component

  alias Livebook.Settings
  alias Livebook.Settings.EnvironmentVariable

  @impl true
  def update(assigns, socket) do
    changeset =
      if assigns.env_var do
        Settings.change_env_var(assigns.env_var)
      else
        Settings.change_env_var(%EnvironmentVariable{})
      end

    {:ok,
     socket
     |> assign(assigns)
     |> assign(changeset: changeset)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        Edit environment variable
      </h3>
      <p class="text-gray-700">
        Configure your application global environment variables.
      </p>
      <.form
        id={"#{@id}-form"}
        let={f}
        for={@changeset}
        phx-target={@myself}
        phx-submit="save"
        phx-change="validate"
        autocomplete="off"
        spellcheck="false"
      >
        <div class="flex flex-col space-y-4">
          <div>
            <div class="input-label">
              Key <span class="text-xs text-gray-500">(alphanumeric and underscore)</span>
            </div>
            <%= text_input(f, :key, class: "input") %>
            <%= error_tag(f, :key) %>
          </div>
          <div>
            <div class="input-label">
              Value
            </div>
            <%= text_input(f, :value, class: "input") %>
            <%= error_tag(f, :value) %>
          </div>

          <div class="flex space-x-2">
            <%= submit("Save",
              class: "button-base button-blue",
              disabled: not @changeset.valid?,
              phx_disabled_with: "Adding..."
            ) %>
            <%= live_patch("Cancel", to: @return_to, class: "button-base button-outlined-gray") %>
          </div>
        </div>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"environment_variable" => attrs}, socket) do
    {:noreply, assign(socket, changeset: Settings.change_env_var(socket.assigns.env_var, attrs))}
  end

  def handle_event("save", %{"environment_variable" => attrs}, socket) do
    case Settings.update_env_var(socket.assigns.env_var, attrs) do
      {:ok, _} ->
        send(self(), {:env_vars_updated, Livebook.Settings.fetch_env_vars()})
        {:noreply, push_patch(socket, to: socket.assigns.return_to)}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end
end

defmodule LivebookWeb.SettingsLive.EnvironmentVariableComponent do
  use LivebookWeb, :live_component

  alias Livebook.Settings
  alias Livebook.Settings.EnvironmentVariable

  @impl true
  def mount(socket) do
    env_var = %EnvironmentVariable{}
    operation = :new
    changeset = Settings.change_env_var(env_var)

    {:ok, assign(socket, changeset: changeset, env_var: env_var, operation: operation)}
  end

  @impl true
  def update(assigns, socket) do
    {env_var, operation} =
      unless assigns.env_var,
        do: {%EnvironmentVariable{}, :new},
        else: {assigns.env_var, :edit}

    changeset = Settings.change_env_var(env_var)

    {:ok,
     socket
     |> assign(assigns)
     |> assign(changeset: changeset, env_var: env_var, operation: operation)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        <%= if @operation == :new, do: "Add environment variable", else: "Edit environment variable" %>
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
          <.input_wrapper form={f} field={:key} class="flex flex-col space-y-1">
            <div class="input-label">
              Key <span class="text-xs text-gray-500">(alphanumeric and underscore)</span>
            </div>
            <%= text_input(f, :key, class: "input") %>
          </.input_wrapper>
          <.input_wrapper form={f} field={:value} class="flex flex-col space-y-1">
            <div class="input-label">Value</div>
            <%= text_input(f, :value, class: "input") %>
          </.input_wrapper>

          <div class="flex space-x-2">
            <%= submit("Save",
              class: "button-base button-blue",
              disabled: not @changeset.valid?,
              phx_disabled_with: "Adding..."
            ) %>
            <button phx-click="cancel" phx-target={@myself} class="button-base button-outlined-gray">
              Cancel
            </button>
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

  def handle_event("cancel", _, socket) do
    {:noreply, push_patch(socket, to: socket.assigns.return_to)}
  end

  def handle_event("save", %{"environment_variable" => attrs}, socket) do
    case {socket.assigns.changeset.valid?, socket.assigns.operation} do
      {true, :new} -> create_env_var(socket, attrs)
      {true, :edit} -> update_env_var(socket, attrs)
      {false, _} -> {:noreply, socket}
    end
  end

  defp create_env_var(socket, attrs) do
    case Settings.create_env_var(attrs) do
      {:ok, _} ->
        send(self(), {:env_vars_updated, Livebook.Settings.fetch_env_vars()})
        {:noreply, push_patch(socket, to: socket.assigns.return_to)}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  defp update_env_var(socket, attrs) do
    case Settings.update_env_var(socket.assigns.env_var, attrs) do
      {:ok, _} ->
        send(self(), {:env_vars_updated, Livebook.Settings.fetch_env_vars()})
        {:noreply, push_patch(socket, to: socket.assigns.return_to)}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end
end

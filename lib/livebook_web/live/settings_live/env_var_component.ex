defmodule LivebookWeb.SettingsLive.EnvVarComponent do
  use LivebookWeb, :live_component

  alias Livebook.Settings
  alias Livebook.Settings.EnvVar

  @impl true
  def update(assigns, socket) do
    {env_var, operation} =
      if assigns.env_var,
        do: {assigns.env_var, :edit},
        else: {%EnvVar{}, :new}

    changeset = Settings.change_env_var(env_var)

    {:ok,
     socket
     |> assign(assigns)
     |> assign(changeset: changeset, env_var: env_var, operation: operation)}
  end

  @impl true
  def render(assigns) do
    assigns =
      assigns
      |> assign_new(:on_save, fn -> "save" end)
      |> assign_new(:resource, fn -> "environment variable" end)
      |> assign_new(:headline, fn ->
        "Configure your application global environment variables."
      end)

    ~H"""
    <div class="p-6 flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        <%= if @operation == :new, do: "Add #{@resource}", else: "Edit #{@resource}" %>
      </h3>
      <p class="text-gray-700">
        <%= @headline %>
      </p>
      <.form
        id={"#{@id}-form"}
        let={f}
        for={@changeset}
        phx-submit={@on_save}
        phx-change="validate"
        autocomplete="off"
        spellcheck="false"
      >
        <div class="flex flex-col space-y-4">
          <.input_wrapper form={f} field={:key} class="flex flex-col space-y-1">
            <div class="input-label">
              Key <span class="text-xs text-gray-500">(alphanumeric and underscore)</span>
            </div>
            <%= text_input(f, :key, class: "input", autofocus: @operation == :new) %>
          </.input_wrapper>
          <.input_wrapper form={f} field={:value} class="flex flex-col space-y-1">
            <div class="input-label">Value</div>
            <%= text_input(f, :value, class: "input", autofocus: @operation == :edit) %>
          </.input_wrapper>

          <div class="flex space-x-2">
            <%= submit("Save",
              class: "button-base button-blue",
              disabled: not @changeset.valid?,
              phx_disabled_with: "Adding..."
            ) %>
            <%= live_patch("Cancel",
              to: @return_to,
              type: "button",
              class: "button-base button-outlined-gray"
            ) %>
          </div>
        </div>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"env_var" => attrs}, socket) do
    {:noreply, assign(socket, changeset: Settings.change_env_var(socket.assigns.env_var, attrs))}
  end
end

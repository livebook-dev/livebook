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
    assigns = assign_new(assigns, :on_save, fn -> "save" end)

    ~H"""
    <div class="p-6 flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        <%= if @operation == :new, do: "Add environment variable", else: "Edit environment variable" %>
      </h3>
      <p class="text-gray-700">
        <%= @headline %>
      </p>
      <.form
        :let={f}
        id={"#{@id}-form"}
        for={@changeset}
        phx-submit={@on_save}
        phx-change={JS.push("validate", target: @myself)}
        autocomplete="off"
        spellcheck="false"
      >
        <div class="flex flex-col space-y-4">
          <.text_field
            field={f[:name]}
            label="Name (alphanumeric and underscore)"
            autofocus={@operation == :new}
            class="uppercase"
          />
          <.text_field field={f[:value]} label="Value" autofocus={@operation == :edit} />
          <.hidden_field field={f[:operation]} value={@operation} />
          <div class="flex space-x-2">
            <button
              class="button-base button-blue"
              type="submit"
              phx-disable-with="Adding..."
              disabled={not @changeset.valid?}
            >
              Save
            </button>
            <.link patch={@return_to} class="button-base button-outlined-gray">
              Cancel
            </.link>
          </div>
        </div>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"env_var" => attrs}, socket) do
    changeset =
      socket.assigns.env_var
      |> Settings.change_env_var(attrs)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, changeset: changeset)}
  end
end

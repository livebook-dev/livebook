defmodule LivebookWeb.Hub.SecretFormComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs
  alias Livebook.Secrets
  alias Livebook.Secrets.Secret

  @impl true
  def mount(socket) do
    {:ok, assign(socket, error_message: nil)}
  end

  @impl true
  def update(assigns, socket) do
    {:ok,
     socket
     |> assign(assigns)
     |> assign_new(:changeset, fn ->
       Secrets.change_secret(%Secret{}, %{
         name: assigns.secret_name,
         value: assigns.secret_value
       })
     end)
     |> assign(
       title: title(socket),
       button: button_attrs(socket),
       deployment_group_id: assigns[:deployment_group_id]
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        {@title}
      </h3>
      <p class="text-gray-700">
        A notebook can read the secret value as a LB_ prefixed environment variable.
      </p>
      <div :if={@error_message} class="error-box">
        {@error_message}
      </div>
      <div class="flex flex-columns gap-4">
        <.form
          :let={f}
          id={"#{@id}-form"}
          for={@changeset}
          phx-target={@myself}
          phx-change="validate"
          phx-submit="save"
          autocomplete="off"
          class="basis-1/2 grow"
        >
          <div class="flex flex-col space-y-4">
            <.text_field
              field={f[:name]}
              label="Name (alphanumeric and underscore)"
              autofocus={@secret_name == nil}
              spellcheck="false"
              autocomplete="off"
              readonly={@secret_name != nil}
              phx-debounce
              help="Name cannot be changed"
              class={secret_name_input_class(@secret_name)}
            />
            <.password_field
              field={f[:value]}
              label="Value"
              autofocus={@secret_name != nil}
              spellcheck="false"
              autocomplete="off"
              phx-debounce
            />
            <.hidden_field field={f[:hub_id]} value={@hub.id} />
            <.hidden_field field={f[:deployment_group_id]} value={@deployment_group_id} />
            <div class="flex space-x-2">
              <.button type="submit" disabled={@disabled or not @changeset.valid?}>
                <.remix_icon icon={@button.icon} />
                <span class="font-normal">{@button.label}</span>
              </.button>
              <.button color="gray" outlined patch={@return_to}>
                Cancel
              </.button>
            </div>
          </div>
        </.form>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("save", %{"secret" => attrs}, socket) do
    changeset = Secrets.change_secret(%Secret{}, attrs)

    with {:ok, secret} <- Ecto.Changeset.apply_action(changeset, :insert),
         :ok <- save_secret(socket, secret, changeset) do
      message =
        if socket.assigns.secret_name,
          do: "Secret #{secret.name} updated successfully",
          else: "Secret #{secret.name} added successfully"

      {:noreply,
       socket
       |> put_flash(:success, message)
       |> push_patch(to: socket.assigns.return_to)}
    else
      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, changeset: changeset)}

      {:transport_error, error} ->
        {:noreply, assign(socket, error_message: error)}
    end
  end

  def handle_event("validate", %{"secret" => attrs}, socket) do
    changeset =
      %Secret{}
      |> Secrets.change_secret(attrs)
      |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, changeset: changeset)}
  end

  defp title(%{assigns: %{secret_name: nil}}), do: "Add secret"
  defp title(_), do: "Edit secret"

  defp button_attrs(%{assigns: %{secret_name: nil}}), do: %{icon: "add-line", label: "Add"}
  defp button_attrs(_), do: %{icon: "save-line", label: "Save"}

  defp save_secret(socket, secret, changeset) do
    result =
      if socket.assigns.secret_name do
        Hubs.update_secret(socket.assigns.hub, secret)
      else
        Hubs.create_secret(socket.assigns.hub, secret)
      end

    with {:error, errors} <- result do
      {:error,
       changeset
       |> Livebook.Utils.put_changeset_errors(errors)
       |> Map.replace!(:action, :validate)}
    end
  end

  defp secret_name_input_class(nil), do: "uppercase"
  defp secret_name_input_class(_), do: "uppercase bg-gray-200/50 border-200/80 cursor-not-allowed"
end

defmodule LivebookWeb.Hub.Teams.DeploymentGroupFormComponent do
  use LivebookWeb, :live_component

  alias Livebook.Teams
  alias Livebook.Teams.DeploymentGroup

  @impl true
  def mount(socket) do
    {:ok, assign(socket, form: nil, error_message: nil)}
  end

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    if socket.assigns.form do
      {:ok, socket}
    else
      {:ok, assign_form(socket, change_deployment_group(socket, %{}))}
    end
  end

  defp assign_form(socket, changeset) do
    assign(socket, :form, to_form(changeset))
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 max-w-4xl flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        Add deployment group
      </h3>
      <div :if={@error_message} class="error-box">
        <%= @error_message %>
      </div>
      <div class="flex flex-columns gap-4">
        <.form
          :let={f}
          id={"#{@id}-form"}
          for={@form}
          phx-target={@myself}
          phx-change="validate"
          phx-submit="save"
          autocomplete="off"
          class="basis-1/2 grow"
        >
          <div class="flex flex-col space-y-4">
            <.text_field
              field={f[:name]}
              label="Name"
              autofocus="true"
              spellcheck="false"
              autocomplete="off"
              phx-debounce
            />
            <.select_field
              label="Mode"
              help={
                ~S'''
                Deployment group mode.
                '''
              }
              field={f[:mode]}
              options={[
                {"Offline", :offline},
                {"Online", :online}
              ]}
            />

            <LivebookWeb.AppComponents.deployment_group_form_content hub={@hub} form={f} />

            <div class="flex space-x-2">
              <.button type="submit" disabled={not @form.source.valid?}>
                <.remix_icon icon="add-line" />
                <span class="font-normal">Add</span>
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
  def handle_event("save", %{"deployment_group" => attrs}, socket) do
    changeset = change_deployment_group(socket, attrs)

    with {:ok, deployment_group} <- Ecto.Changeset.apply_action(changeset, :update),
         {:ok, _id} <- Teams.create_deployment_group(socket.assigns.hub, deployment_group) do
      {:noreply,
       socket
       |> put_flash(:success, "Deployment group added successfully")
       |> push_patch(to: ~p"/hub/#{socket.assigns.hub.id}")}
    else
      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign_form(socket, Map.replace!(changeset, :action, :validate))}

      {:transport_error, message} ->
        {:noreply, assign(socket, error_message: message)}

      {:error, message} ->
        {:noreply, assign(socket, error_message: message)}
    end
  end

  def handle_event("validate", %{"deployment_group" => attrs}, socket) do
    changeset =
      change_deployment_group(socket, attrs)
      |> Map.replace!(:action, :validate)

    {:noreply, assign_form(socket, changeset)}
  end

  defp change_deployment_group(socket, attrs) do
    %DeploymentGroup{hub_id: socket.assigns.hub.id}
    |> Teams.change_deployment_group(attrs)
  end
end

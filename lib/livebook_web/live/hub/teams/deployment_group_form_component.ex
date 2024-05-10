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
          id={"#{@id}-form"}
          for={@form}
          phx-target={@myself}
          phx-change="validate"
          phx-submit="save"
          autocomplete="off"
          class="basis-1/2 grow"
        >
          <div class="flex flex-col space-y-4">
            <div>
              <label class="mb-2 flex items-center gap-1 text-sm text-gray-800 font-medium">
                Type
              </label>
              <div class="flex gap-y-6 sm:gap-x-4">
                <.radio_card field={@form[:mode]} title="Online" value={:online}>
                  Deploy Livebook apps to your infrastructure with the click of a button.
                  This mode requires running app servers connected to Livebook Teams.
                </.radio_card>

                <.radio_card field={@form[:mode]} title="Airgapped" value={:offline}>
                  Manually deploy Livebook apps to your infrastructure via Dockerfiles.
                  Connection to Livebook Teams is not required.
                </.radio_card>
              </div>
            </div>

            <.text_field
              field={@form[:name]}
              label="Name"
              autofocus="true"
              spellcheck="false"
              autocomplete="off"
              phx-debounce
            />

            <LivebookWeb.AppComponents.deployment_group_form_content hub={@hub} form={@form} />

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

  defp radio_card(assigns) do
    ~H"""
    <label class={[
      "relative flex cursor-pointer rounded-lg border bg-white p-4 shadow-sm focus:outline-none w-1/2",
      to_string(@field.value) == to_string(@value) &&
        "border-blue-500 ring-1 ring-blue-500"
    ]}>
      <input
        type="radio"
        name={@field.name}
        value={@value}
        checked={to_string(@field.value) == to_string(@value)}
        class="sr-only"
      />
      <span class="flex flex-1">
        <span class="flex flex-col">
          <span class="block text-sm font-medium text-gray-900">
            <%= @title %>
          </span>
          <span class="mt-1 flex items-center text-sm text-gray-700">
            <%= render_slot(@inner_block) %>
          </span>
        </span>
      </span>
      <.remix_icon
        icon="checkbox-circle-fill"
        class={[
          "text-blue-600 h-5 w-5",
          if(to_string(@field.value) == to_string(@value), do: "visible", else: "invisible")
        ]}
      />
      <span class="pointer-events-none absolute -inset-px rounded-lg border-2" aria-hidden="true">
      </span>
    </label>
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

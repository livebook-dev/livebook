defmodule LivebookWeb.Hub.Teams.DeploymentGroupFormComponent do
  use LivebookWeb, :live_component

  alias Livebook.Teams.DeploymentGroups.DeploymentGroup
  alias Livebook.Teams.DeploymentGroups

  @impl true
  def update(assigns, socket) do
    deployment_group = assigns.deployment_group

    deployment_group = deployment_group || %DeploymentGroup{hub_id: assigns.hub.id}
    changeset = DeploymentGroups.change_deployment_group(deployment_group)

    socket = assign(socket, assigns)

    {:ok,
     assign(socket,
       deployment_group: deployment_group,
       changeset: changeset,
       mode: mode(deployment_group),
       title: title(deployment_group),
       button: button(deployment_group),
       error_message: nil
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 max-w-4xl flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        <%= @title %>
      </h3>
      <div class="flex flex-columns gap-4">
        <.form
          :let={f}
          id={"#{@id}-form"}
          for={to_form(@changeset, as: :deployment_group)}
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
                {"Offline", "offline"},
                {"Online", "online"}
              ]}
            />
            <div class="flex space-x-2">
              <button class="button-base button-blue" type="submit" disabled={not @changeset.valid?}>
                <.remix_icon icon={@button.icon} class="align-middle mr-1" />
                <span class="font-normal"><%= @button.label %></span>
              </button>
              <.link patch={@return_to} class="button-base button-outlined-gray">
                Cancel
              </.link>
            </div>
          </div>
        </.form>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("save", %{"deployment_group" => attrs}, socket) do
    with {:ok, deployment_group} <-
           DeploymentGroups.update_deployment_group(socket.assigns.deployment_group, attrs),
         {:ok, _id} <- save_deployment_group(deployment_group, socket) do
      message =
        case socket.assigns.mode do
          :new -> "Deployment group #{deployment_group.name} added successfully"
          :edit -> "Deployment group #{deployment_group.name} updated successfully"
        end

      {:noreply,
       socket
       |> put_flash(:success, message)
       |> push_redirect(to: socket.assigns.return_to)}
    else
      {:error, %Ecto.Changeset{} = changeset} -> {:noreply, assign(socket, changeset: changeset)}
      {:transport_error, message} -> {:noreply, assign(socket, error_message: message)}
      {:error, message} -> {:noreply, assign(socket, error_message: message)}
    end
  end

  def handle_event("validate", %{"deployment_group" => attrs}, socket) do
    changeset =
      %DeploymentGroup{}
      |> DeploymentGroup.changeset(attrs)
      |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, changeset: changeset)}
  end

  defp save_deployment_group(deployment_group, socket) do
    case socket.assigns.mode do
      :new -> DeploymentGroups.create_deployment_group(socket.assigns.hub, deployment_group)
      :edit -> DeploymentGroups.update_deployment_group(socket.assigns.hub, deployment_group)
    end
  end

  defp mode(%DeploymentGroup{name: nil}), do: :new
  defp mode(_), do: :edit

  defp title(%DeploymentGroup{name: nil}), do: "Add deployment group"
  defp title(_), do: "Edit deployment group"

  defp button(%DeploymentGroup{name: nil}), do: %{icon: "add-line", label: "Add"}
  defp button(_), do: %{icon: "save-line", label: "Save"}
end

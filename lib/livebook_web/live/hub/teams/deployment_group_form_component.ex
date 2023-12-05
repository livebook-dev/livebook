defmodule LivebookWeb.Hub.Teams.DeploymentGroupFormComponent do
  use LivebookWeb, :live_component

  alias Livebook.Teams.DeploymentGroup
  alias Livebook.Teams

  @impl true
  def update(assigns, socket) do
    deployment_group = assigns.deployment_group
    hub = assigns.hub

    deployment_group = deployment_group || %DeploymentGroup{hub_id: assigns.hub.id}
    changeset = Teams.change_deployment_group(deployment_group)

    socket = assign(socket, assigns)

    {:ok,
     assign(socket,
       deployment_group: deployment_group,
       changeset: changeset,
       mode: mode(deployment_group),
       title: title(deployment_group),
       button: button(deployment_group),
       subtitle: subtitle(deployment_group, hub.hub_name),
       error_message: nil
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="max-w-4xl flex flex-col space-y-5">
      <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
        <%= @title %>
      </h2>

      <p class="text-gray-700">
        <%= @subtitle %>
      </p>
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
              <%= if @mode == :new do %>
                <.link patch={@return_to} class="button-base button-outlined-gray">
                  Cancel
                </.link>
              <% end %>
            </div>
          </div>
        </.form>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("save", %{"deployment_group" => attrs}, socket) do
    changeset = Teams.change_deployment_group(socket.assigns.deployment_group, attrs)

    with {:ok, deployment_group} <- Ecto.Changeset.apply_action(changeset, :update),
         {:ok, id} <- save_deployment_group(deployment_group, socket) do
      message =
        case socket.assigns.mode do
          :new -> "Deployment group #{deployment_group.name} added successfully"
          :edit -> "Deployment group #{deployment_group.name} updated successfully"
        end

      {:noreply,
       socket
       |> put_flash(:success, message)
       |> push_patch(to: ~p"/hub/#{socket.assigns.hub.id}/deployment-groups/edit/#{id}")}
    else
      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, changeset: changeset)}

      {:transport_error, message} ->
        {:noreply, assign(socket, error_message: message)}

      {:error, message} ->
        {:noreply, assign(socket, error_message: message)}
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
      :new -> Teams.create_deployment_group(socket.assigns.hub, deployment_group)
      :edit -> Teams.update_deployment_group(socket.assigns.hub, deployment_group)
    end
  end

  defp mode(%DeploymentGroup{name: nil}), do: :new
  defp mode(_), do: :edit

  defp title(%DeploymentGroup{name: nil}), do: "Add deployment group"
  defp title(_), do: "Edit deployment group"

  defp subtitle(%DeploymentGroup{name: nil}, hub_name),
    do: "Add a new deployment group to #{hub_name}"

  defp subtitle(%DeploymentGroup{name: deployment_group}, _),
    do: "Manage the #{deployment_group} deployment group"

  defp button(%DeploymentGroup{name: nil}), do: %{icon: "add-line", label: "Add"}
  defp button(_), do: %{icon: "save-line", label: "Save"}
end

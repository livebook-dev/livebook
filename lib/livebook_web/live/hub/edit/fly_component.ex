defmodule LivebookWeb.Hub.Edit.FlyComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs.{Fly, FlyClient}

  @impl true
  def update(assigns, socket) do
    changeset = Fly.change_hub(assigns.hub)
    {:ok, app} = FlyClient.fetch_app(assigns.hub)
    env_vars = env_vars_from_secrets(app["secrets"])

    env_var =
      if name = assigns.env_var_id do
        Enum.find(env_vars, &(&1.name == name))
      end

    {:ok,
     socket
     |> assign(assigns)
     |> assign(
       app_url: "https://fly.io/apps/#{app["name"]}",
       changeset: changeset,
       env_vars: env_vars,
       env_var: env_var
     )}
  end

  defp env_vars_from_secrets(secrets) do
    for secret <- secrets do
      %Livebook.Settings.EnvVar{name: secret["name"]}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={"#{@id}-component"}>
      <div class="flex flex-col space-y-10">
        <div class="flex flex-col space-y-2">
          <div class="flex items-center justify-between border border-gray-200 rounded-lg p-4">
            <div class="flex items-center space-x-12">
              <.labeled_text label="Application ID">
                <%= @hub.application_id %>
              </.labeled_text>
              <.labeled_text label="Type">
                Fly
              </.labeled_text>
            </div>

            <a href={@app_url} class="button-base button-outlined-gray" target="_blank">
              <.remix_icon icon="dashboard-2-line" class="align-middle mr-1" />
              <span>Manage app on Fly</span>
            </a>
          </div>
        </div>

        <div class="flex flex-col space-y-2">
          <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
            General
          </h2>

          <.form
            :let={f}
            id={@id}
            class="flex flex-col mt-4 space-y-4"
            for={@changeset}
            phx-submit="save"
            phx-change="validate"
            phx-target={@myself}
            phx-debounce="blur"
          >
            <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
              <.input_wrapper form={f} field={:hub_name} class="flex flex-col space-y-1">
                <div class="input-label">Name</div>
                <%= text_input(f, :hub_name, class: "input") %>
              </.input_wrapper>

              <.input_wrapper form={f} field={:hub_emoji} class="flex flex-col space-y-1">
                <div class="input-label">Emoji</div>
                <.emoji_input
                  id="fly-emoji-input"
                  form={f}
                  field={:hub_emoji}
                  container_class="mt-10"
                />
              </.input_wrapper>
            </div>

            <%= submit("Update Hub",
              class: "button-base button-blue",
              phx_disable_with: "Updating...",
              disabled: not @changeset.valid?
            ) %>
          </.form>
        </div>

        <div class="flex flex-col space-y-4">
          <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
            Environment Variables
          </h2>

          <.live_component
            module={LivebookWeb.EnvVarsComponent}
            id="env-vars"
            env_vars={@env_vars}
            return_to={Routes.hub_path(@socket, :edit, @hub.id)}
            add_env_var_path={Routes.hub_path(@socket, :add_env_var, @hub.id)}
            edit_label="Replace"
            target={@myself}
          />
        </div>
      </div>

      <%= if @live_action in [:add_env_var, :edit_env_var] do %>
        <.modal
          id="env-var-modal"
          show
          class="w-full max-w-3xl"
          target={@myself}
          patch={Routes.hub_path(@socket, :edit, @hub.id)}
        >
          <.live_component
            module={LivebookWeb.EnvVarComponent}
            id="env-var"
            on_save={JS.push("save_env_var", target: @myself)}
            env_var={@env_var}
            headline="Configute your Fly application system environment variables"
            return_to={Routes.hub_path(@socket, :edit, @hub.id)}
          />
        </.modal>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_event("save", %{"fly" => params}, socket) do
    case Fly.update_hub(socket.assigns.hub, params) do
      {:ok, hub} ->
        {:noreply,
         socket
         |> put_flash(:success, "Hub updated successfully")
         |> push_redirect(to: Routes.hub_path(socket, :edit, hub.id))}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  def handle_event("validate", %{"fly" => attrs}, socket) do
    {:noreply, assign(socket, changeset: Fly.change_hub(socket.assigns.hub, attrs))}
  end

  # EnvVar component callbacks

  def handle_event("save_env_var", %{"env_var" => attrs}, socket) do
    env_operation = attrs["operation"]
    attrs = %{"key" => attrs["name"], "value" => attrs["value"]}

    case FlyClient.set_secrets(socket.assigns.hub, [attrs]) do
      {:ok, _} ->
        message =
          if env_operation == "new",
            do: "Environment variable added",
            else: "Environment variable updated"

        {:noreply,
         socket
         |> put_flash(:success, message)
         |> push_redirect(to: Routes.hub_path(socket, :edit, socket.assigns.hub.id))}

      {:error, _} ->
        message =
          if env_operation == "new",
            do: "Failed to add environment variable",
            else: "Failed to update environment variable"

        {:noreply,
         socket
         |> put_flash(:error, message)
         |> push_redirect(to: Routes.hub_path(socket, :edit, socket.assigns.hub.id))}
    end
  end

  def handle_event("edit_env_var", %{"env_var" => key}, socket) do
    {:noreply,
     push_patch(socket, to: Routes.hub_path(socket, :edit_env_var, socket.assigns.hub.id, key))}
  end

  def handle_event("delete_env_var", %{"env_var" => key}, socket) do
    case FlyClient.unset_secrets(socket.assigns.hub, [key]) do
      {:ok, _} ->
        {:noreply,
         socket
         |> put_flash(:success, "Environment variable deleted")
         |> push_redirect(to: Routes.hub_path(socket, :edit, socket.assigns.hub.id))}

      {:error, _} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete environment variable")
         |> push_redirect(to: Routes.hub_path(socket, :edit, socket.assigns.hub.id))}
    end
  end
end

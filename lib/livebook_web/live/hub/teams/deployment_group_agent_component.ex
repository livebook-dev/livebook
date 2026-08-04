defmodule LivebookWeb.Hub.Teams.DeploymentGroupAgentComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs
  alias Livebook.Teams.DeploymentGroupInstructions

  @impl true
  def mount(socket) do
    {:ok, assign(socket, messages: [], hide_title: false)}
  end

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    socket =
      if socket.assigns[:agent_key_id] do
        socket
      else
        agent_key_id =
          case assigns.deployment_group.agent_keys do
            [%{id: id} | _] -> id
            _ -> nil
          end

        assign(socket, :agent_key_id, agent_key_id)
      end

    socket =
      assign_new(socket, :changeset, fn ->
        Hubs.Dockerfile.config_changeset(base_config(socket))
      end)

    {:ok, update_instructions(socket)}
  end

  defp base_config(socket) do
    Hubs.Dockerfile.from_deployment_group(socket.assigns.deployment_group)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col gap-3">
      <h3 :if={not @hide_title} class="text-2xl font-semibold text-gray-800">
        App server setup
      </h3>

      <p class="text-gray-700 mb-2">
        App servers are instances of Livebook running inside your infrastructure
        that you deploy your apps to. Follow the instructions below to start new instances.
      </p>

      <div :if={@messages != []} class="flex flex-col gap-2">
        <.message_box :for={{kind, message} <- @messages} kind={kind}>
          {raw(message)}
        </.message_box>
      </div>

      <.form :let={f} for={@changeset} as={:data} phx-change="validate" phx-target={@myself}>
        <.radio_field
          label="Base Docker image"
          field={f[:docker_tag]}
          options={LivebookWeb.AppComponents.docker_tag_options()}
        />
      </.form>

      <.form
        :let={f}
        :if={match?([_, _ | _], @deployment_group.agent_keys)}
        for={%{"id" => @agent_key_id}}
        as={:agent_key}
        phx-change="select_agent_key"
        phx-target={@myself}
      >
        <div class="grid grid-cols-1 md:grid-cols-2">
          <.select_field
            label="Server key"
            field={f[:id]}
            options={
              for key <- @deployment_group.agent_keys do
                {value_preview(key.key), key.id}
              end
            }
          />
        </div>
      </.form>

      <%= if @agent_key_id do %>
        <div class="mt-2">
          <.tabs id="deployment-instruction" default="docker">
            <:tab id="docker" label="Docker">
              <div class="flex flex-col gap-3">
                <p class="text-gray-700">
                  Deploy an app server to any Docker-based infrastructure. You may want
                  to set the environment variables as secrets, if applicable. Below is
                  an example calling Docker CLI directly, adapt it as necessary.
                </p>

                <.code_preview_with_title_and_copy
                  title="CLI"
                  source_id="agent-dockerfile-source"
                  source={@instructions.docker_instructions}
                  language="shell"
                />
              </div>
            </:tab>
            <:tab id="fly_io" label="Fly.io">
              <div class="flex flex-col gap-3">
                <p class="text-gray-700">
                  Deploy an app server to Fly.io with a few commands. Let's first configure the application resources:
                </p>

                <.code_preview_with_title_and_copy
                  title="CLI"
                  source_id="agent-fly-source-1"
                  source={@instructions.fly_instructions.step_one}
                  language="shell"
                />

                <p class="text-gray-700">
                  Now let's set secrets and deploy it:
                </p>

                <.code_preview_with_title_and_copy
                  title="CLI"
                  source_id="agent-fly-source-2"
                  source={@instructions.fly_instructions.step_two}
                  language="shell"
                />
              </div>
            </:tab>
            <:tab id="k8s" label="Kubernetes">
              <div class="flex flex-col gap-3">
                <p class="text-gray-700">
                  Deploy an app server to Kubernetes. First save the following Kubernetes resource file to disk:
                </p>

                <.code_preview_with_title_and_copy
                  title="livebook.yml"
                  source_id="agent-k8s-source"
                  source={@instructions.k8s_instructions}
                  language="yaml"
                />

                <p class="text-gray-700">
                  Now run the following shell command:
                </p>

                <.code_preview_with_title_and_copy
                  title="CLI"
                  source_id="agent-k8s-cli"
                  source="kubectl apply -f livebook.yml"
                  language="shell"
                />
              </div>
            </:tab>
          </.tabs>
        </div>
      <% end %>
    </div>
    """
  end

  defp value_preview(string) do
    preview_length = 10
    length = String.length(string)
    String.slice(string, 0, preview_length) <> String.duplicate("•", length - preview_length)
  end

  @impl true
  def handle_event("select_agent_key", %{"agent_key" => %{"id" => id}}, socket) do
    id = if(id != "", do: id)
    {:noreply, assign(socket, agent_key_id: id) |> update_instructions()}
  end

  def handle_event("validate", %{"data" => data}, socket) do
    changeset =
      socket
      |> base_config()
      |> Hubs.Dockerfile.config_changeset(data)
      |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, changeset: changeset) |> update_instructions()}
  end

  defp update_instructions(socket) do
    config = Ecto.Changeset.apply_changes(socket.assigns.changeset)
    warnings = Hubs.Dockerfile.online_warnings(config)
    messages = Enum.map(warnings, &{"warning", &1})
    assign(socket, instructions: instructions(socket), messages: messages)
  end

  defp instructions(%{assigns: %{agent_key_id: nil}}), do: nil

  defp instructions(socket) do
    hub = socket.assigns.hub
    deployment_group = socket.assigns.deployment_group

    agent_key =
      Enum.find(deployment_group.agent_keys, &(&1.id == socket.assigns.agent_key_id))

    config = Ecto.Changeset.apply_changes(socket.assigns.changeset)

    %{image: image, env: env} =
      Livebook.Hubs.Dockerfile.online_docker_info(config, hub, agent_key)

    %{
      docker_instructions: DeploymentGroupInstructions.docker(image, env),
      fly_instructions:
        DeploymentGroupInstructions.fly(image, env, hub.hub_name, deployment_group.name),
      k8s_instructions: DeploymentGroupInstructions.k8s(image, env, deployment_group.name)
    }
  end
end

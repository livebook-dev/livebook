defmodule LivebookWeb.Hub.Teams.DeploymentGroupInstanceComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs

  @impl true
  def mount(socket) do
    {:ok, assign(socket, messages: [])}
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
    <div class="p-6 max-w-4xl flex flex-col space-y-3">
      <h3 class="text-2xl font-semibold text-gray-800">
        App server setup
      </h3>

      <p class="text-gray-700">
        App server is an instance of Livebook where you can deploy Livebook
        apps via Livebook Teams. Each app server belongs to a specific
        deployment group.
      </p>

      <.content
        hub={@hub}
        deployment_group={@deployment_group}
        myself={@myself}
        agent_key_id={@agent_key_id}
        changeset={@changeset}
        instructions={@instructions}
        messages={@messages}
      />
    </div>
    """
  end

  defp content(%{deployment_group: %{agent_keys: []}} = assigns) do
    ~H"""
    <.message_box kind={:info}>
      This deployment group has no server keys, you can add one <.link
        href={Livebook.Config.teams_url() <> "/orgs/#{@hub.org_id}/deployments/groups/#{@deployment_group.id}/server-keys"}
        class="text-sm font-semibold text-blue-600  hover:text-blue-700"
        target="_blank"
      >
        on Teams <.remix_icon icon="external-link-line" />
      </.link>.
    </.message_box>
    """
  end

  defp content(assigns) do
    ~H"""
    <div class="flex flex-col gap-3">
      <p class="text-gray-700">
        Use the instructions below to set up an instance in your own infrastructure.
        Once the instance is running, it will connect to Livebook Teams and become
        available for app deployments.
      </p>

      <div :if={@messages != []} class="flex flex-col gap-2">
        <.message_box :for={{kind, message} <- @messages} kind={kind}>
          <%= raw(message) %>
        </.message_box>
      </div>

      <.form
        :let={f}
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

      <.form :let={f} for={@changeset} as={:data} phx-change="validate" phx-target={@myself}>
        <.radio_field
          label="Base image"
          field={f[:docker_tag]}
          options={LivebookWeb.AppComponents.docker_tag_options()}
        />
      </.form>

      <%= if @agent_key_id do %>
        <div class="mt-5">
          <.tabs id="deployment-instruction" default={default_tab(@deployment_group)}>
            <:tab id="docker" label="Docker">
              <div class="flex flex-col gap-3">
                <p class="text-gray-700">
                  Deploy an app server to any Docker-based infrastructure. You may want
                  to set the environment variables as secrets, if applicable. Below is
                  an example calling Docker CLI directly, adapt it as necessary.
                </p>
                <div>
                  <div class="flex items-end mb-1 gap-1">
                    <span class="text-sm text-gray-700 font-semibold">CLI</span>
                  </div>

                  <.code_preview
                    source_id="agent-dockerfile-source"
                    source={@instructions.docker_instructions}
                    language="shell"
                  />
                </div>
              </div>
            </:tab>
            <:tab id="fly_io" label="Fly.io">
              <div class="flex flex-col gap-3">
                <p class="text-gray-700">
                  Deploy an app server to Fly.io with a few simple commands.
                </p>
                <div>
                  <div class="flex items-end mb-1 gap-1">
                    <span class="text-sm text-gray-700 font-semibold">CLI</span>
                  </div>

                  <.code_preview
                    source_id="agent-dockerfile-source"
                    source={@instructions.fly_instructions}
                    language="shell"
                  />
                </div>
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

  defp default_tab(%{clustering: :fly_io}), do: "fly_io"
  defp default_tab(_deloyment_group), do: "docker"

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
    warnings = Hubs.Dockerfile.agent_warnings(config)
    messages = Enum.map(warnings, &{:warning, &1})
    assign(socket, instructions: instructions(socket), messages: messages)
  end

  defp instructions(%{assigns: %{agent_key_id: nil}}), do: nil

  defp instructions(socket) do
    hub = socket.assigns.hub

    agent_key =
      Enum.find(
        socket.assigns.deployment_group.agent_keys,
        &(&1.id == socket.assigns.agent_key_id)
      )

    config = Ecto.Changeset.apply_changes(socket.assigns.changeset)

    %{image: image, env: env} = Livebook.Hubs.Dockerfile.agent_docker_info(config, hub, agent_key)

    %{
      docker_instructions: docker_instructions(image, env),
      fly_instructions: fly_instructions(image, env)
    }
  end

  defp docker_instructions(image, env) do
    envs = Enum.map_join(env, "\n", fn {key, value} -> ~s/  -e #{key}="#{value}" \\/ end)

    """
    docker run -p 8080:8080 -p 8081:8081 --pull \\
    #{envs}
      #{image}
    """
  end

  defp fly_instructions(image, env) do
    envs = Enum.map_join(env, " \\\n", fn {key, value} -> ~s/  #{key}="#{value}"/ end)

    """
    APP_NAME="my_name"

    flyctl apps create $APP_NAME

    flyctl secrets set --app $APP_NAME \\
    #{envs}

    flyctl deploy --app $APP_NAME --image #{image}
    """
  end
end

defmodule Livebook.Teams.DeploymentGroupInstructions do
  # This module is responsible for building instructions for deploying
  # an app server.
  #
  # Note that the environment variables are configured on Livebook Teams
  # and synchronised to every client, so we always encode them, such
  # that they cannot alter the generated commands and resources.

  require EEx

  @doc """
  Generates a Docker CLI command starting an app server.
  """
  @spec docker(String.t(), list({String.t(), String.t()})) :: String.t()
  def docker(image, env) do
    envs =
      Enum.map_join(env, "\n", fn {key, value} ->
        ~s/  -e #{Livebook.Utils.shell_quote("#{key}=#{value}")} \\/
      end)

    """
    docker run -p 8080:8080 -p 8081:8081 --pull always \\
    #{envs}
      #{image}
    """
  end

  @doc """
  Generates Fly.io CLI commands deploying an app server.
  """
  @spec fly(String.t(), list({String.t(), String.t()}), String.t(), String.t()) :: %{
          step_one: String.t(),
          step_two: String.t()
        }
  def fly(image, env, hub_name, deployment_group_name) do
    envs =
      Enum.map_join(env, " \\\n", fn {key, value} ->
        "  " <> Livebook.Utils.shell_quote("#{key}=#{value}")
      end)

    example_dir =
      "lb-server-#{hub_name}-#{deployment_group_name}"
      |> String.replace(~r/[^\w\-]/, "")
      |> String.downcase()

    %{
      step_one: """
      mkdir #{example_dir}
      cd #{example_dir}
      fly launch --image #{image} --vm-memory 2048 --no-deploy
      """,
      step_two: """
      fly secrets set \\
      #{envs}

      fly deploy --ha=false
      """
    }
  end

  @doc """
  Generates a Kubernetes resource file deploying an app server.
  """
  @spec k8s(String.t(), list({String.t(), String.t()}), String.t()) :: String.t()
  def k8s(image, env, deployment_group_name) do
    {secrets, envs} =
      Map.split(
        Map.new(env),
        ~w(LIVEBOOK_TEAMS_KEY LIVEBOOK_TEAMS_AUTH LIVEBOOK_SECRET_KEY_BASE LIVEBOOK_COOKIE)
      )

    # We replace auto by the cluster setting.
    {replicas, envs} =
      case envs do
        %{"LIVEBOOK_CLUSTER" => "auto"} -> {2, Map.delete(envs, "LIVEBOOK_CLUSTER")}
        %{} -> {1, envs}
      end

    envs =
      Map.put_new(
        envs,
        "LIVEBOOK_CLUSTER",
        "dns:livebook-headless.$(POD_NAMESPACE).svc.cluster.local"
      )

    dg_suffix = sanitize_for_node_name(deployment_group_name)

    k8s_template(image, envs, secrets, replicas, dg_suffix)
  end

  EEx.function_from_string(
    :defp,
    :k8s_template,
    """
    apiVersion: v1
    kind: Service
    metadata:
      name: livebook-headless
    spec:
      clusterIP: None
      selector:
        app: livebook

    ---

    apiVersion: v1
    kind: Service
    metadata:
      name: livebook-loadbalancer
    spec:
      type: LoadBalancer
      ports:
        - port: 80
          targetPort: 8080
      selector:
        app: livebook

    ---

    apiVersion: apps/v1
    kind: Deployment
    metadata:
      name: livebook
    spec:
      replicas: <%= replicas %>
      selector:
        matchLabels:
          app: livebook
      template:
        metadata:
          labels:
            app: livebook
        spec:
          containers:
            - name: livebook
              image: <%= image %>
              ports:
                - containerPort: 8080
              env:
                - name: POD_IP
                  valueFrom:
                    fieldRef:
                      fieldPath: status.podIP
                - name: POD_NAMESPACE
                  valueFrom:
                    fieldRef:
                      fieldPath: metadata.namespace
                - name: LIVEBOOK_NODE
                  value: "livebook-<%= dg_suffix %>@$(POD_IP)"<%= for {k, v} <- envs, k != "LIVEBOOK_NODE" do %>
                - name: <%= inspect(k) %>
                  value: <%= inspect(v) %><% end %><%= for {k, _} <- secrets do %>
                - name: <%= k %>
                  valueFrom:
                    secretKeyRef:
                      name: livebook-secret
                      key: <%= k %><% end %>

    ---

    apiVersion: v1
    kind: Secret
    metadata:
      name: livebook-secret
    type: Opaque
    data:
      # Notice the values below are Base64 encoded
      # LIVEBOOK_PASSWORD: <base64_encoded_password><%= for {k, v} <- secrets do %>
      <%= k %>: <%= Base.encode64(v) %><% end %>
    """,
    [:image, :envs, :secrets, :replicas, :dg_suffix]
  )

  defp sanitize_for_node_name(string) do
    sanitized =
      string
      |> String.downcase()
      |> String.replace(~r/[^a-z0-9]/, "_")
      |> String.replace(~r/_+/, "_")
      |> String.trim("_")
      |> String.slice(0, 40)
      |> String.trim("_")

    if sanitized == "" do
      string
      |> Base.encode32(padding: false, case: :lower)
      |> String.slice(0, 20)
    else
      sanitized
    end
  end
end

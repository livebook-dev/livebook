defmodule Livebook.K8s.Pod do
  @main_container_name "livebook-runtime"
  @pvc_name_volume_name "livebook-home"

  @default_pod_template """
  apiVersion: v1
  kind: Pod
  metadata:
    generateName: livebook-runtime-
  spec:
    containers:
      - name: livebook-runtime
        resources:
          limits:
            cpu: "1"
            memory: 1Gi
          requests:
            cpu: "1"
            memory: 1Gi\
  """

  @doc """
  Returns the default pod template.
  """
  @spec default_pod_template() :: String.t()
  def default_pod_template(), do: @default_pod_template

  @doc """
  Set the namespace on the given manifest.
  """
  @spec set_namespace(map(), String.t()) :: map()
  def set_namespace(manifest, namespace) do
    put_in(manifest, ["metadata", "namespace"], namespace)
  end

  @doc """
  Adds "volume" and "volumeMount" configurations to `manifest` in order
  to mount `pvc_name` under /home/livebook on the pod.
  """
  @spec set_pvc_name(map(), String.t()) :: map()
  def set_pvc_name(manifest, pvc_name) do
    manifest
    |> update_in(["spec", Access.key("volumes", [])], fn volumes ->
      volume = %{
        "name" => @pvc_name_volume_name,
        "persistentVolumeClaim" => %{"claimName" => pvc_name}
      }

      [volume | volumes]
    end)
    |> update_in(
      ["spec", "containers", access_main_container(), Access.key("volumeMounts", [])],
      fn volume_mounts ->
        [%{"name" => @pvc_name_volume_name, "mountPath" => "/home/livebook"} | volume_mounts]
      end
    )
  end

  @doc """
  Adds the list of `env_vars` to the main container of the given `manifest`.
  """
  @spec add_env_vars(map(), list()) :: map()
  def add_env_vars(manifest, env_vars) do
    update_in(
      manifest,
      ["spec", "containers", access_main_container(), Access.key("env", [])],
      fn existing_vars -> env_vars ++ existing_vars end
    )
  end

  @doc """
  Sets the tag of the main container's image.
  """
  @spec set_docker_tag(map(), String.t()) :: map()
  def set_docker_tag(manifest, docker_tag) do
    image = "ghcr.io/livebook-dev/livebook:#{docker_tag}"
    put_in(manifest, ["spec", "containers", access_main_container(), "image"], image)
  end

  @doc """
  Adds the `port` to the main container and adds a readiness probe.
  """
  @spec add_container_port(map(), non_neg_integer()) :: map()
  def add_container_port(manifest, port) do
    readiness_probe = %{
      "tcpSocket" => %{"port" => port},
      "initialDelaySeconds" => 1,
      "periodSeconds" => 1
    }

    manifest
    |> update_in(
      ["spec", "containers", access_main_container(), Access.key("ports", [])],
      &[%{"containerPort" => port} | &1]
    )
    |> put_in(["spec", "containers", access_main_container(), "readinessProbe"], readiness_probe)
  end

  @doc """
  Turns the given `pod_template` into a Pod manifest.
  """
  @spec pod_from_template(String.t()) :: map()
  def pod_from_template(pod_template) do
    pod_template
    |> YamlElixir.read_from_string!()
    |> do_pod_from_template()
  end

  defp do_pod_from_template(pod) do
    pod
    |> Map.merge(%{"apiVersion" => "v1", "kind" => "Pod"})
    |> put_in(["spec", "restartPolicy"], "Never")
  end

  @doc """
  Validates the given Pod manifest.
  """
  @spec validate_pod_template(map(), String.t()) :: :ok | {:error, String.t()}
  def validate_pod_template(pod, namespace)

  def validate_pod_template(%{"apiVersion" => "v1", "kind" => "Pod"} = pod, namespace) do
    with :ok <- validate_basics(pod),
         :ok <- validate_main_container(pod),
         :ok <- validate_namespace(pod, namespace) do
      validate_container_image(pod)
    end
  end

  def validate_pod_template(_other_input, _namespace) do
    {:error, ~s/Make sure to define a valid resource of apiVersion "v1" and kind "Pod"./}
  end

  defp validate_basics(pod) do
    cond do
      not match?(%{"metadata" => %{}}, pod) ->
        {:error, ".metadata is missing in your pod template."}

      not match?(%{"spec" => %{"containers" => containers}} when is_list(containers), pod) ->
        {:error, ".spec.containers is missing in your pod template."}

      pod["metadata"]["name"] in [nil, ""] and pod["metadata"]["generateName"] in [nil, ""] ->
        {:error,
         "Make sure to define .metadata.name or .metadata.generateName in your pod template."}

      true ->
        :ok
    end
  end

  defp validate_main_container(pod) do
    if get_in(pod, ["spec", "containers", access_main_container()]) do
      :ok
    else
      {:error,
       ~s/Main container is missing. The main container should be named "#{@main_container_name}"./}
    end
  end

  defp validate_container_image(pod) do
    if get_in(pod, ["spec", "containers", access_main_container(), "image"]) do
      {:error,
       "You can't set the container image of the main container. It's going to be overridden."}
    else
      :ok
    end
  end

  defp validate_namespace(pod, namespace) do
    template_ns = get_in(pod, ["metadata", "namespace"])

    if template_ns == nil or template_ns == namespace do
      :ok
    else
      {:error,
       "The field .template.metadata.namespace has to be omitted or set to the namespace you selected."}
    end
  end

  defp access_main_container() do
    Access.find(&(&1["name"] == @main_container_name))
  end
end

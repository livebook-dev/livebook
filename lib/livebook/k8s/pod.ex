defmodule Livebook.K8s.Pod do
  @main_container_name "livebook-runtime"
  @home_pvc_volume_name "lb-home-folder"
  @default_pod_template """
  apiVersion: v1
  kind: PodTemplate
  metadata:
    name: livebook-runtime-template
  template:
    metadata:
      generateName: livebook-runtime-
    spec:
      containers:
        - image: ghcr.io/livebook-dev/livebook:nightly
          name: livebook-runtime
          resources:
            limits:
              cpu: "1"
              memory: 1Gi
            requests:
              cpu: "1"
              memory: 1Gi
  """

  defmacrop access_main_container() do
    quote do
      Access.filter(&(&1["name"] == @main_container_name))
    end
  end

  defguardp is_empty(value) when is_nil(value) or value == "" or value == []

  def default_pod_template(), do: @default_pod_template

  def set_namespace(manifest, namespace) do
    put_in(manifest, ~w(metadata namespace), namespace)
  end

  def set_home_pvc(manifest, home_pvc) when is_empty(home_pvc), do: manifest

  def set_home_pvc(manifest, home_pvc) do
    manifest
    |> update_in(["spec", Access.key("volumes", [])], fn volumes ->
      volume = %{
        "name" => @home_pvc_volume_name,
        "persistentVolumeClaim" => %{"claimName" => home_pvc}
      }

      [volume | volumes]
    end)
    |> update_in(
      ["spec", "containers", access_main_container(), Access.key("volumeMounts", [])],
      fn volume_mounts ->
        [%{"name" => @home_pvc_volume_name, "mountPath" => "/home/livebook"} | volume_mounts]
      end
    )
  end

  def add_env_vars(manifest, env_vars) do
    update_in(
      manifest,
      ["spec", "containers", access_main_container(), Access.key("env", [])],
      fn existing_vars -> env_vars ++ existing_vars end
    )
  end

  def set_docker_tag(manifest, docker_tag) do
    image = "ghcr.io/livebook-dev/livebook:#{docker_tag}"
    put_in(manifest, ["spec", "containers", access_main_container(), "image"], image)
  end

  def pod_from_template(pod_template) do
    pod =
      pod_template
      |> YamlElixir.read_from_string!()
      |> Map.get("template")

    pod
    |> Map.merge(%{"apiVersion" => "v1", "kind" => "Pod"})
    |> put_in(~w(spec restartPolicy), "Never")
  end

  def validate_pod_template(pod_template, namespace) do
    with :ok <- validate_basics(pod_template),
         :ok <- validate_main_container(pod_template),
         :ok <- validate_namespace(pod_template, namespace) do
      validate_container_image(pod_template)
    end
  end

  defp validate_basics(pod_template) do
    cond do
      pod_template["apiVersion"] != "v1" or pod_template["kind"] != "PodTemplate" ->
        {:error,
         ~s'Make sure to define a valid resource of apiVersion "v1" and kind "PodTemplate"'}

      !is_map(pod_template["template"]) or !is_map(pod_template["template"]["metadata"]) ->
        {:error, ".template.metadata is missing"}

      !is_map(pod_template["template"]["spec"]) or
          !is_list(pod_template["template"]["spec"]["containers"]) ->
        {:error, ".template.spec.containers is missing"}

      is_empty(pod_template["template"]["metadata"]["name"]) and
          is_empty(pod_template["template"]["metadata"]["generateName"]) ->
        {:error,
         ~s'Make sure to define .template.metadata.name or .template.metadata.generateName'}

      true ->
        :ok
    end
  end

  defp validate_main_container(pod_template) do
    if get_in(pod_template, ["template", "spec", "containers", access_main_container()]) == [] do
      {:error,
       "Main container is missing. The main container should be named '#{@main_container_name}'."}
    else
      :ok
    end
  end

  defp validate_container_image(pod_template) do
    image =
      pod_template
      |> get_in(["template", "spec", "containers", access_main_container(), "image"])
      |> List.first()

    case image do
      nil ->
        :ok

      "ghcr.io/livebook-dev/livebook:" <> _tag ->
        :ok

      custom_image ->
        supported_images =
          Livebook.Config.docker_images()
          |> Enum.map(&"ghcr.io/livebook-dev/livebook:#{&1.tag}")
          |> Enum.join(", ")

        {:warning,
         "Make sure your container image '#{custom_image}' is based off on of the following image: #{supported_images}"}
    end
  end

  defp validate_namespace(pod_template, namespace) do
    ns = get_in(pod_template, ~w(metadata namespace))
    template_ns = get_in(pod_template, ~w(template metadata namespace))

    cond do
      !is_nil(ns) and ns != namespace ->
        {:error,
         "The field .metadata.namespace has to be omitted or set to the namespace you selected."}

      !is_nil(template_ns) and template_ns != namespace ->
        {:error,
         "The field .template.metadata.namespace has to be omitted or set to the namespace you selected."}

      true ->
        :ok
    end
  end
end

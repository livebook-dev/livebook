defmodule Livebook.K8s.Pod do
  import Ecto.Changeset

  @main_container_name "livebook-runtime"
  @home_pvc_volume_name "lb-home-folder"

  defmacrop access_by_name(name) do
    quote do
      Access.filter(&(&1["name"] == unquote(name)))
    end
  end

  defguardp is_empty(value) when is_nil(value) or value == ""

  defmodule Resources do
    use Ecto.Schema

    @type t :: %__MODULE__{
            cpu: String.t(),
            memory: String.t(),
            gpu: String.t()
          }

    @primary_key false
    embedded_schema do
      field :cpu, :string, default: "1"
      field :memory, :string, default: "1Gi"
      field :gpu, :string
    end

    @fields ~w(cpu memory gpu)a
    @required ~w(cpu memory)a

    def changeset(resources, attrs) do
      resources
      |> cast(attrs, @fields)
      |> validate_required(@required)
    end

    def build_manifest(resources) do
      resources
      |> Map.new(fn
        {:gpu, value} -> {"nvidia/gpu", value}
        {key, value} -> {Atom.to_string(key), value}
      end)
    end

    def parse_manifest(manifest) do
      %{
        cpu: Map.get(manifest, "cpu"),
        memory: Map.get(manifest, "memory"),
        gpu: Map.get(manifest, "nvidia/gpu")
      }
    end
  end

  defmodule BasicSpecs do
    use Ecto.Schema

    @type t :: %__MODULE__{
            docker_tag: String.t(),
            resource_limits: Resources.t(),
            resource_requests: Resources.t()
          }

    @primary_key false

    embedded_schema do
      field :docker_tag, :string, default: hd(Livebook.Config.docker_images()).tag
      embeds_one :resource_limits, Resources, on_replace: :update
      embeds_one :resource_requests, Resources, on_replace: :update
    end

    @fields ~w(
    docker_tag
  )a

    def changeset(attrs \\ %{}) do
      %__MODULE__{resource_limits: %Resources{}, resource_requests: %Resources{}}
      |> cast(attrs, @fields)
      |> validate_required(@fields)
      |> cast_embed(:resource_limits)
      |> cast_embed(:resource_requests)
    end
  end

  defmodule AdvancedSpecs do
    alias Livebook.K8s.KeyValue
    use Ecto.Schema

    @type t :: %__MODULE__{
            annotations: [KeyValue.t()],
            labels: [KeyValue.t()],
            service_account_name: String.t()
          }

    @primary_key false

    embedded_schema do
      field :service_account_name, :string, default: "default"
      embeds_many :labels, KeyValue
      embeds_many :annotations, KeyValue
    end

    @fields ~w(
      service_account_name
    )a

    def changeset(attrs \\ %{}) do
      %__MODULE__{}
      |> cast(attrs, @fields)
      |> validate_required(@fields)
      |> cast_embed(:labels,
        sort_param: :labels_sort,
        drop_param: :labels_drop
      )
      |> cast_embed(:annotations,
        sort_param: :annotations_sort,
        drop_param: :annotations_drop
      )
    end
  end

  def build_manifest(namespace, basic_specs, advanced_specs, home_pvc) do
    manifest = %{
      "apiVersion" => "v1",
      "kind" => "Pod",
      "metadata" => %{
        "namespace" => namespace
      },
      "spec" => %{
        "serviceAccountName" => advanced_specs.service_account_name,
        "restartPolicy" => "Never",
        "containers" => [
          %{
            "name" => @main_container_name,
            "image" => "ghcr.io/livebook-dev/livebook:#{basic_specs.docker_tag}",
            "resources" => %{
              "requests" => Resources.build_manifest(basic_specs.resource_requests),
              "limits" => Resources.build_manifest(basic_specs.resource_limits)
            },
            "env" => [
              %{
                "name" => "POD_IP",
                "valueFrom" => %{
                  "fieldRef" => %{"apiVersion" => "v1", "fieldPath" => "status.podIP"}
                }
              }
            ]
          }
        ]
      }
    }

    manifest
    |> add_metadata("labels", advanced_specs.labels)
    |> add_metadata("annotations", advanced_specs.annotations)
    |> set_home_pvc(home_pvc)
  end

  def parse_manifest(manifest) do
    main_container =
      manifest
      |> get_in(["spec", "containers", access_by_name(@main_container_name)])
      |> List.first()

    [_, docker_tag] = main_container |> Map.get("image") |> String.split(":")

    home_pvc =
      manifest
      |> get_in([
        "specs",
        "volumes",
        access_by_name(@home_pvc_volume_name),
        "persistentVolumeClaim",
        "claimName"
      ])
      |> List.wrap()
      |> List.first()

    basic_specs = %{
      docker_tag: docker_tag,
      resource_limits: Resources.parse_manifest(get_in(main_container, ["resources", "limits"])),
      resource_requests:
        Resources.parse_manifest(get_in(main_container, ["resources", "requests"]))
    }

    advanced_specs = %{
      annotations: get_in(manifest, ~w(metadata annotations)),
      labels: get_in(manifest, ~w(metadata labels)),
      service_account_name: get_in(manifest, ~w(spec serviceAccountName))
    }

    %{home_pvc: home_pvc, basic_specs: basic_specs, advanced_specs: advanced_specs}
  end

  defp set_home_pvc(manifest, home_pvc) when is_empty(home_pvc), do: manifest

  defp set_home_pvc(manifest, home_pvc) do
    manifest
    |> update_in(["spec", Access.key("volumes", [])], fn volumes ->
      volume = %{
        "name" => @home_pvc_volume_name,
        "persistentVolumeClaim" => %{"claimName" => home_pvc}
      }

      [volume | volumes]
    end)
    |> update_in(
      ["spec", "containers", Access.filter(&(&1["name"] == @main_container_name))],
      fn container ->
        container
        |> update_in([Access.key("volumeMounts", [])], fn volume_mounts ->
          [%{"name" => @home_pvc_volume_name, "mountPath" => "/home/livebook"} | volume_mounts]
        end)
      end
    )
  end

  defp add_metadata(manifest, _field, kv) when is_nil(kv) or kv == [], do: manifest

  defp add_metadata(manifest, field, kv) do
    update_in(manifest, ["metadata", Access.key(field, %{})], fn existing ->
      for %{key: key, value: value} <- kv, reduce: existing do
        acc -> Map.put(acc, key, value)
      end
    end)
  end

  def add_env_vars(manifest, env_vars) do
    update_in(
      manifest,
      ["spec", "containers", access_by_name(@main_container_name), Access.key("env", [])],
      fn existing_vars -> env_vars ++ existing_vars end
    )
  end
end

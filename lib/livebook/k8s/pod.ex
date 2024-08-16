defmodule Livebook.K8s.Pod do
  import Ecto.Changeset

  @main_container_name "livebook-runtime"

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
      embeds_one :resource_limits, Resources
      embeds_one :resource_requests, Resources
    end

    @fields ~w(
    docker_tag
  )a

    def changeset(manifest, attrs \\ %{}) do
      manifest
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

    def changeset(manifest, attrs \\ %{}) do
      manifest
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
      "appVersion" => "v1",
      "kind" => "Pod",
      "metadata" => %{
        "namespace" => namespace
      },
      "spec" => %{
        "containers" => [
          %{
            "name" => @main_container_name,
            "image" => "ghcr.io/livebook-dev/livebook:#{basic_specs.docker_tag}",
            "resources" => %{
              "requests" => basic_specs.resource_requests,
              "limits" => basic_specs.resource_limits
            },
            "serviceAccountName" => advanced_specs.service_account_name
          }
        ]
      }
    }

    manifest
    |> add_metadata("labels", advanced_specs.labels)
    |> add_metadata("annotations", advanced_specs.annotations)
    |> set_home_pvc(home_pvc)
  end

  defp set_home_pvc(manifest, nil), do: manifest

  defp set_home_pvc(manifest, home_pvc) do
    volume_name = "lb-home-folder"

    manifest
    |> update_in([Access.key("volumes", [])], fn volumes ->
      volume = %{
        "name" => volume_name,
        "persistentVolumeClaim" => %{"claimName" => home_pvc}
      }

      [volume | volumes]
    end)
    |> update_in(
      ["spec", "containers", Access.filter(&(&1["name"] == @main_container_name))],
      fn container ->
        container
        |> update_in([Access.key("volumeMounts", [])], fn volume_mounts ->
          [%{"name" => volume_name, "mountPath" => "/home/livebook"} | volume_mounts]
        end)
      end
    )
  end

  defp add_metadata(manifest, field, kv) when is_nil(kv) or kv == [], do: manifest

  defp add_metadata(manifest, field, kv) do
    update_in(manifest, ["metadata", Access.key(field, [])], fn existing -> kv ++ existing end)
  end
end

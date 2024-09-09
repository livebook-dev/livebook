defmodule Livebook.K8s.PVC do
  use Ecto.Schema

  import Ecto.Changeset

  @type t :: %__MODULE__{
          name: String.t(),
          size_gb: integer(),
          access_mode: String.t(),
          storage_class: String.t()
        }

  @primary_key false
  embedded_schema do
    field :name, :string
    field :size_gb, :integer
    field :access_mode, :string, default: "ReadWriteOnce"
    field :storage_class, :string, default: nil
  end

  @fields ~w(name size_gb access_mode storage_class)a
  @required ~w(name size_gb access_mode)a

  @doc """
  Build a PVC changeset for the given `attrs`.
  """
  @spec changeset(map()) :: Ecto.Changeset.t()
  def changeset(attrs \\ %{}) do
    %__MODULE__{}
    |> cast(attrs, @fields)
    |> validate_required(@required)
  end

  @doc """
  Build PVC manifest for the given `pvc` and `namespace` to be applied to a
  cluster.
  """
  @spec manifest(pvc :: t(), namespace: String.t()) :: manifest :: map()
  def manifest(pvc, namespace) do
    %{
      "apiVersion" => "v1",
      "kind" => "PersistentVolumeClaim",
      "metadata" => %{
        "name" => pvc.name,
        "namespace" => namespace
      },
      "spec" => %{
        "storageClassName" => pvc.storage_class,
        "accessModes" => [pvc.access_mode],
        "resources" => %{
          "requests" => %{
            "storage" => "#{pvc.size_gb}Gi"
          }
        }
      }
    }
  end
end

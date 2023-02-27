defmodule Livebook.Notebook.AppSettings do
  @moduledoc false

  use Ecto.Schema

  import Ecto.Changeset, except: [change: 1, change: 2]

  @type t :: %__MODULE__{
          slug: String.t() | nil,
          access_type: access_type(),
          password: String.t() | nil,
          show_source: boolean()
        }

  @type access_type :: :public | :protected

  @primary_key false
  embedded_schema do
    field :slug, :string
    field :access_type, Ecto.Enum, values: [:public, :protected]
    field :password, :string
    field :show_source, :boolean
  end

  @doc """
  Returns default app settings.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      slug: nil,
      access_type: :protected,
      password: generate_password(),
      show_source: false
    }
  end

  defp generate_password() do
    :crypto.strong_rand_bytes(10) |> Base.encode32(case: :lower)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking settings changes.
  """
  @spec change(t(), map()) :: Ecto.Changeset.t()
  def change(%__MODULE__{} = settings, attrs \\ %{}) do
    changeset(settings, attrs)
  end

  @doc """
  Updates settings with the given changes.
  """
  @spec update(t(), map()) :: {:ok, t()} | {:error, Ecto.Changeset.t()}
  def update(%__MODULE__{} = settings, attrs) do
    changeset = changeset(settings, attrs)
    apply_action(changeset, :update)
  end

  defp changeset(settings, attrs) do
    settings
    |> cast(attrs, [:slug, :access_type, :show_source])
    |> validate_required([:slug, :access_type, :show_source])
    |> validate_format(:slug, ~r/^[a-zA-Z0-9-]+$/,
      message: "slug can only contain alphanumeric characters and dashes"
    )
    |> cast_access_attrs(attrs)
  end

  defp cast_access_attrs(changeset, attrs) do
    case get_field(changeset, :access_type) do
      :protected ->
        changeset
        |> cast(attrs, [:password])
        |> validate_required([:password])
        |> validate_length(:password, min: 12)

      _other ->
        changeset
    end
  end

  @doc """
  Checks if the app settings are complete and valid.
  """
  @spec valid?(t()) :: boolean()
  def valid?(settings) do
    change(settings).valid?
  end
end

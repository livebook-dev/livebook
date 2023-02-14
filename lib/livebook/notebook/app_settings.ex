defmodule Livebook.Notebook.AppSettings do
  @moduledoc false

  use Ecto.Schema

  import Ecto.Changeset, except: [change: 1]

  @type t :: %__MODULE__{
          slug: String.t() | nil
        }

  @primary_key false
  embedded_schema do
    field :slug, :string
  end

  @doc """
  Returns default app settings.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{slug: nil}
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking settings changes.
  """
  @spec change(t(), map()) :: Ecto.Changeset.t()
  def change(%__MODULE__{} = settings, attrs \\ %{}) do
    settings
    |> changeset(attrs)
    |> Map.put(:action, :validate)
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
    |> cast(attrs, [:slug])
    |> validate_required([:slug])
    |> validate_format(:slug, ~r/^[a-zA-Z0-9-]+$/,
      message: "slug can only contain alphanumeric characters and dashes"
    )
  end

  @doc """
  Checks if the app settings are complete and valid.
  """
  @spec valid?(t()) :: boolean()
  def valid?(settings) do
    change(settings).valid?
  end
end

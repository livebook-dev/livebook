defmodule Livebook.Notebook.AppSettings do
  @moduledoc false

  # Data structure configuring how notebook gets deployed as an app.

  use Ecto.Schema

  import Ecto.Changeset, except: [change: 1, change: 2]

  @type t :: %__MODULE__{
          slug: String.t() | nil,
          multi_session: boolean(),
          zero_downtime: boolean(),
          show_existing_sessions: boolean(),
          auto_shutdown_ms: pos_integer() | nil,
          access_type: access_type(),
          password: String.t() | nil,
          show_source: boolean(),
          output_type: output_type()
        }

  @type access_type :: :public | :protected
  @type output_type :: :all | :rich

  @primary_key false
  embedded_schema do
    field :slug, :string
    field :multi_session, :boolean
    field :zero_downtime, :boolean
    field :show_existing_sessions, :boolean
    field :auto_shutdown_ms, :integer
    field :access_type, Ecto.Enum, values: [:public, :protected]
    field :password, :string
    field :show_source, :boolean
    field :output_type, Ecto.Enum, values: [:all, :rich]
  end

  @doc """
  Returns default app settings.
  """
  @spec new() :: t()
  def new() do
    %__MODULE__{
      slug: nil,
      multi_session: false,
      zero_downtime: false,
      show_existing_sessions: true,
      auto_shutdown_ms: nil,
      access_type: :protected,
      password: generate_password(),
      show_source: false,
      output_type: :all
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
    |> cast(attrs, [
      :slug,
      :multi_session,
      :auto_shutdown_ms,
      :access_type,
      :show_source,
      :output_type
    ])
    |> validate_required([
      :slug,
      :multi_session,
      :access_type,
      :show_source,
      :output_type
    ])
    |> validate_format(:slug, ~r/^[a-zA-Z0-9-]+$/,
      message: "should only contain alphanumeric characters and dashes"
    )
    |> cast_access_attrs(attrs)
    |> cast_mode_specific_attrs(attrs)
    |> put_defaults()
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

  defp cast_mode_specific_attrs(changeset, attrs) do
    case get_field(changeset, :multi_session) do
      false ->
        changeset
        |> cast(attrs, [:zero_downtime])
        |> validate_required([:zero_downtime])
        # Listing sessions is not applicable to single-session apps,
        # since they have a single session at a time
        |> put_change(:show_existing_sessions, true)

      true ->
        changeset
        |> cast(attrs, [:show_existing_sessions])
        |> validate_required([:show_existing_sessions])
        # Zero-downtime deployment is not applicable to multi-session
        # apps, since they are inherently zero-downtime. We reset to
        # the default, so we do not persist it unnecessarily
        |> put_change(:zero_downtime, false)
    end
  end

  defp put_defaults(changeset) do
    case get_change(changeset, :multi_session) do
      nil ->
        changeset

      false ->
        changeset
        |> put_change(:auto_shutdown_ms, nil)

      true ->
        changeset
        |> put_change(:auto_shutdown_ms, 5_000)
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

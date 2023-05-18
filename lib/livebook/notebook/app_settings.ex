defmodule Livebook.Notebook.AppSettings do
  @moduledoc false

  use Ecto.Schema

  import Ecto.Changeset, except: [change: 1, change: 2]

  @type t :: %__MODULE__{
          slug: String.t() | nil,
          multi_session: boolean(),
          zero_downtime: boolean(),
          auto_session_startup: boolean(),
          auto_shutdown_type: auto_shutdown_type(),
          access_type: access_type(),
          password: String.t() | nil,
          show_source: boolean(),
          output_type: output_type()
        }

  @type auto_shutdown_type :: :never | :inactive_5s | :inactive_1m | :inactive_1h | :new_version
  @type access_type :: :public | :protected
  @type output_type :: :all | :rich

  @primary_key false
  embedded_schema do
    field :slug, :string
    field :multi_session, :boolean
    field :zero_downtime, :boolean
    field :auto_session_startup, :boolean

    field :auto_shutdown_type, Ecto.Enum,
      values: [:never, :inactive_5s, :inactive_1m, :inactive_1h, :new_version]

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
      auto_session_startup: false,
      auto_shutdown_type: :new_version,
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
      :auto_shutdown_type,
      :access_type,
      :show_source,
      :output_type
    ])
    |> validate_required([
      :slug,
      :multi_session,
      :auto_shutdown_type,
      :access_type,
      :show_source,
      :output_type
    ])
    |> validate_format(:slug, ~r/^[a-zA-Z0-9-]+$/,
      message: "slug can only contain alphanumeric characters and dashes"
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
        # Automatic startup is not applicable to single-session apps,
        # since they have a single session and it is always started
        # automatically
        |> put_change(:auto_session_startup, false)

      true ->
        changeset
        |> cast(attrs, [:auto_session_startup])
        |> validate_required([:auto_session_startup])
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
        |> put_change(:auto_shutdown_type, :new_version)

      true ->
        changeset
        |> put_change(:auto_shutdown_type, :inactive_5s)
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

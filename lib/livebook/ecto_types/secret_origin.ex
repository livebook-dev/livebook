defmodule Livebook.EctoTypes.SecretOrigin do
  @moduledoc false
  use Ecto.Type

  @type t :: :system_env | :startup | :app | <<_::64, _::_*8>>

  def type, do: :string

  def load("system_env"), do: {:ok, :system_env}
  def load("app"), do: {:ok, :app}
  def load("startup"), do: {:ok, :startup}

  def load(id) when is_binary(id) do
    if hub_secret?(id),
      do: {:ok, id},
      else: :error
  end

  def load(_), do: :error

  def dump(:system_env), do: {:ok, "system_env"}
  def dump(:app), do: {:ok, "app"}
  def dump(:startup), do: {:ok, "startup"}
  def dump("enterprise-" <> _ = id), do: {:ok, id}

  def dump(id) when is_binary(id) do
    if hub_secret?(id), do: {:ok, id}, else: :error
  end

  def dump(_), do: :error

  def cast(:system_env), do: {:ok, :system_env}
  def cast(:app), do: {:ok, :app}
  def cast(:startup), do: {:ok, :startup}

  def cast(id) when is_binary(id) do
    if hub_secret?(id),
      do: {:ok, id},
      else: {:error, message: "does not exists"}
  end

  def cast(_), do: {:error, message: "is invalid"}

  defdelegate hub_secret?(id), to: Livebook.Hubs, as: :hub_exists?
end

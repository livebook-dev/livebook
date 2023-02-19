defmodule Livebook.EctoTypes.SecretOrigin do
  @moduledoc false
  use Ecto.Type

  @type t :: nil | :session | :startup | :app | {:hub, String.t()}

  def type, do: :string

  def load("session"), do: {:ok, :session}
  def load("app"), do: {:ok, :app}
  def load("startup"), do: {:ok, :startup}

  def load("hub-" <> id) do
    if hub_secret?(id),
      do: {:ok, {:hub, id}},
      else: :error
  end

  def load(_), do: :error

  def dump(:session), do: {:ok, "session"}
  def dump(:app), do: {:ok, "app"}
  def dump(:startup), do: {:ok, "startup"}

  def dump({:hub, id}) when is_binary(id) do
    if hub_secret?(id), do: {:ok, "hub-#{id}"}, else: :error
  end

  def dump(_), do: :error

  def cast(:session), do: {:ok, :session}
  def cast(:app), do: {:ok, :app}
  def cast(:startup), do: {:ok, :startup}
  def cast({:hub, id}) when is_binary(id), do: cast(id)

  def cast(id) when is_binary(id) do
    if hub_secret?(id),
      do: {:ok, {:hub, id}},
      else: {:error, message: "does not exists"}
  end

  def cast(_), do: {:error, message: "is invalid"}

  defdelegate hub_secret?(id), to: Livebook.Hubs, as: :hub_exists?
end

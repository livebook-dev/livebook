defmodule Livebook.EctoTypes.SecretOrigin do
  @moduledoc false

  use Ecto.Type

  @type t :: nil | :session | :startup | :app | {:hub, String.t()}

  @impl true
  def type, do: :string

  @impl true
  def load(origin), do: decode(origin)

  @impl true
  def dump(:session), do: {:ok, "session"}
  def dump(:app), do: {:ok, "app"}
  def dump(:startup), do: {:ok, "startup"}
  def dump({:hub, id}), do: {:ok, "hub-#{id}"}
  def dump(_), do: :error

  @impl true
  def cast(:session), do: {:ok, :session}
  def cast(:app), do: {:ok, :app}
  def cast(:startup), do: {:ok, :startup}
  def cast({:hub, id}), do: {:hub, id}

  def cast(encoded) when is_binary(encoded) do
    case decode(encoded) do
      {:ok, origin} -> {:ok, origin}
      :error -> {:error, message: "is invalid"}
    end
  end

  def cast(_), do: {:error, message: "is invalid"}

  @doc """
  Encodes origin into string representation.
  """
  @spec encode(t()) :: String.t()
  def encode(:session), do: "session"
  def encode(:app), do: "app"
  def encode(:startup), do: "startup"
  def encode({:hub, id}), do: "hub-#{id}"

  @doc """
  Decodes origin from string representation.
  """
  @spec decode(String.t()) :: {:ok, t()} | :error
  def decode("session"), do: {:ok, :session}
  def decode("app"), do: {:ok, :app}
  def decode("startup"), do: {:ok, :startup}
  def decode("hub-" <> id), do: {:ok, {:hub, id}}
  def decode(_other), do: :error
end

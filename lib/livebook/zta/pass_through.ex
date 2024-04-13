defmodule Livebook.ZTA.PassThrough do
  @behaviour Livebook.ZTA

  @impl true
  def child_spec(_opts) do
    %{id: __MODULE__, start: {Function, :identity, [:ignore]}}
  end

  @impl true
  def authenticate(_, conn, _) do
    {conn, %{}}
  end
end

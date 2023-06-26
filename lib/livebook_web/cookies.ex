defmodule LivebookWeb.Cookies do
  # This module implements the ZTA contract specific to Livebook cookies
  @moduledoc false

  def authenticate(_, _, _) do
    %{}
  end

  def child_spec(_opts) do
    %{id: __MODULE__, start: {Function, :identity, [:ignore]}}
  end
end

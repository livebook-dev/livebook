defmodule Livebook.Settings do
  @moduledoc false
  
  # Keeps all Livebook settings that are backed by storage.

  @doc """
  Returns the autosave path.

  TODO: Make this configurable in the UI.
  """
  def autosave_path() do
    case Livebook.Storage.current().fetch_key(:settings, "global", :autosave_path) do
      {:ok, value} -> value
      :error -> Path.join(Livebook.Config.data_path(), "autosaved")
    end
  end
end

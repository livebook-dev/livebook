defmodule Livebook.Apps do
  @moduledoc false

  alias Livebook.Session

  @doc """
  Registers a app session under the given slug.

  In case another app is already registered under the given slug,
  this function atomically replaces the registration and instructs
  the previous app to shut down.
  """
  @spec register(pid(), String.t()) :: :ok
  def register(session_pid, slug) do
    name = name(slug)

    :global.trans({{:app_registration, name}, node()}, fn ->
      case :global.whereis_name(name) do
        :undefined ->
          :ok

        pid ->
          :global.unregister_name(name)
          Session.app_shutdown(pid)
      end

      :yes = :global.register_name(name, session_pid)
    end)

    :ok
  end

  @doc """
  Checks if app with the given slug exists.
  """
  @spec exists?(String.t()) :: boolean()
  def exists?(slug) do
    :global.whereis_name(name(slug)) != :undefined
  end

  @doc """
  Looks up app session with the given slug.
  """
  @spec fetch_session_by_slug(String.t()) :: {:ok, Session.t()} | :error
  def fetch_session_by_slug(slug) do
    case :global.whereis_name(name(slug)) do
      :undefined ->
        :error

      pid ->
        session = Session.get_by_pid(pid)
        {:ok, session}
    end
  end

  @doc """
  Looks up app session with the given slug and returns its settings.
  """
  @spec fetch_settings_by_slug(String.t()) :: {:ok, Livebook.Notebook.AppSettings.t()} | :error
  def fetch_settings_by_slug(slug) do
    case :global.whereis_name(name(slug)) do
      :undefined ->
        :error

      pid ->
        app_settings = Session.get_app_settings(pid)
        {:ok, app_settings}
    end
  end

  defp name(slug), do: {:app, slug}
end

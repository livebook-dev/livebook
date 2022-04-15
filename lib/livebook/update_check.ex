defmodule Livebook.UpdateCheck do
  @moduledoc false

  # Periodically checks for available Livebook update.

  use GenServer

  require Logger

  @version_term __MODULE__

  @hour_in_ms 60 * 60 * 1000
  @day_in_ms 24 * @hour_in_ms

  @doc false
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, {})
  end

  @doc """
  Returns the latest Livebook version if it's more recent than the
  current one.
  """
  @spec new_version() :: String.t() | nil
  def new_version() do
    current_version = Application.spec(:livebook, :vsn) |> List.to_string()
    latest_version = get_version()

    if latest_version && Version.compare(current_version, latest_version) == :lt do
      latest_version
    else
      nil
    end
  end

  @impl true
  def init({}) do
    send(self(), :check)
    {:ok, %{}}
  end

  @impl true
  def handle_info(:check, state) do
    case check_release() do
      {:ok, version} ->
        put_version(version)
        Process.send_after(self(), :check, @day_in_ms)

      {:error, error} ->
        Logger.error("version check failed, #{error}")
        Process.send_after(self(), :check, @hour_in_ms)
    end

    {:noreply, state, :hibernate}
  end

  defp check_release() do
    url = "https://api.github.com/repos/livebook-dev/livebook/releases/latest"
    headers = [{"accept", "application/vnd.github.v3+json"}]

    case Livebook.Utils.HTTP.request(:get, url, headers: headers) do
      {:ok, status, _headers, body} ->
        with 200 <- status,
             {:ok, data} <- Jason.decode(body),
             %{"tag_name" => "v" <> version} <- data do
          {:ok, version}
        else
          _ -> {:error, "unexpected response"}
        end

      {:error, reason} ->
        {:error, "failed to make a request, reason: #{inspect(reason)}"}
    end
  end

  defp put_version(version) do
    :persistent_term.put(@version_term, version)
  end

  defp get_version() do
    :persistent_term.get(@version_term, nil)
  end
end

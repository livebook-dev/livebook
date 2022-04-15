defmodule Livebook.UpdateCheck do
  @moduledoc false

  # Periodically checks for available Livebook update.

  use GenServer

  require Logger

  @name __MODULE__
  @timeout :infinity

  @hour_in_ms 60 * 60 * 1000
  @day_in_ms 24 * @hour_in_ms

  @doc false
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, {}, name: @name)
  end

  @doc """
  Returns the latest Livebook version if it's more recent than the
  current one.
  """
  @spec new_version() :: String.t() | nil
  def new_version() do
    GenServer.call(@name, :get_new_version, @timeout)
  end

  @doc """
  Returns whether the update check is enabled.
  """
  @spec enabled?() :: boolean()
  def enabled?() do
    GenServer.call(@name, :get_enabled, @timeout)
  end

  @doc """
  Sets whether the update check is enabled.
  """
  @spec set_enabled(boolean()) :: :ok
  def set_enabled(enabled) do
    GenServer.cast(@name, {:set_enabled, enabled})
  end

  @impl true
  def init({}) do
    send(self(), :check)

    {:ok,
     %{
       enabled: Livebook.Settings.update_check_enabled?(),
       new_version: nil,
       timer_ref: nil
     }}
  end

  @impl true
  def handle_cast({:set_enabled, enabled}, state) do
    Livebook.Settings.set_update_check_enabled(enabled)

    state = cancel_timer(state)

    if enabled do
      send(self(), :check)
    end

    {:noreply, %{state | enabled: enabled}}
  end

  @impl true
  def handle_call(:get_enabled, _from, state) do
    {:reply, state.enabled, state}
  end

  @impl true
  def handle_call(:get_new_version, _from, state) do
    new_version = if state.enabled, do: state.new_version
    {:reply, new_version, state}
  end

  @impl true
  def handle_info(:check, state) do
    state =
      if state.enabled do
        case fetch_latest_version() do
          {:ok, version} ->
            new_version = if newer?(version), do: version
            timer_ref = Process.send_after(self(), :check, @day_in_ms)
            %{state | new_version: new_version, timer_ref: timer_ref}

          {:error, error} ->
            Logger.error("version check failed, #{error}")
            timer_ref = Process.send_after(self(), :check, @hour_in_ms)
            %{state | timer_ref: timer_ref}
        end
      else
        state
      end

    {:noreply, state}
  end

  defp cancel_timer(%{timer_ref: nil} = state), do: state

  defp cancel_timer(state) do
    if Process.cancel_timer(state.timer_ref) == false do
      receive do
        :check -> :ok
      end
    end

    %{state | timer_ref: nil}
  end

  defp fetch_latest_version() do
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

  defp newer?(version) do
    current_version = Application.spec(:livebook, :vsn) |> List.to_string()
    stable?(version) and Version.compare(current_version, version) == :lt
  end

  defp stable?(version) do
    case Version.parse(version) do
      {:ok, %{pre: []}} -> true
      _ -> false
    end
  end
end

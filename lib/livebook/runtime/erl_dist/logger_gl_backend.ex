defmodule Livebook.Runtime.ErlDist.LoggerGLBackend do
  @moduledoc false

  # A logger backend used to forward logs to Livebook,
  # as with regular output.
  #
  # The backend is based on `Logger.Backends.Console`,
  # but instead of logging to the console, it sends
  # log output to the group leader of the soruce process,
  # provided the group leader is an instance of
  # `Livebook.Evaluator.IOProxy`.
  #
  # Basic configuration is taken from the console
  # logger configuration to match its formatting.

  @behaviour :gen_event

  @type state :: %{
          colors: keyword(),
          format: binary(),
          level: atom(),
          metadata: list(atom())
        }

  @impl true
  def init(__MODULE__) do
    config = Application.get_env(:logger, :console)
    {:ok, init_state(config)}
  end

  @impl true
  def handle_event({level, gl, {Logger, msg, ts, md}}, state) do
    %{level: log_level} = state

    if meet_level?(level, log_level) do
      log_event(level, msg, ts, md, gl, state)
    end

    {:ok, state}
  end

  def handle_event(_, state) do
    {:ok, state}
  end

  @impl true
  def code_change(_old_vsn, state, _extra) do
    {:ok, state}
  end

  @impl true
  def handle_call(_message, state) do
    {:ok, :ok, state}
  end

  @impl true
  def handle_info(_message, state) do
    {:ok, state}
  end

  @impl true
  def terminate(_reason, _state) do
    :ok
  end

  defp init_state(config) do
    level = Keyword.get(config, :level)
    format = Logger.Formatter.compile(Keyword.get(config, :format))
    colors = configure_colors(config)
    metadata = Keyword.get(config, :metadata, []) |> configure_metadata()

    %{format: format, metadata: metadata, level: level, colors: colors}
  end

  defp configure_metadata(:all), do: :all
  defp configure_metadata(metadata), do: Enum.reverse(metadata)

  defp configure_colors(config) do
    colors = Keyword.get(config, :colors, [])

    %{
      debug: Keyword.get(colors, :debug, :cyan),
      info: Keyword.get(colors, :info, :normal),
      warn: Keyword.get(colors, :warn, :yellow),
      error: Keyword.get(colors, :error, :red),
      enabled: Keyword.get(colors, :enabled, IO.ANSI.enabled?())
    }
  end

  defp meet_level?(_lvl, nil), do: true

  defp meet_level?(lvl, min) do
    Logger.compare_levels(lvl, min) != :lt
  end

  defp log_event(level, msg, ts, md, gl, state) do
    if io_proxy?(gl) do
      output = format_event(level, msg, ts, md, state)
      async_io(gl, output)
    end
  end

  defp io_proxy?(pid) do
    info = Process.info(pid, [:dictionary])
    info[:dictionary][:"$initial_call"] == {Livebook.Evaluator.IOProxy, :init, 1}
  end

  defp async_io(name, output) when is_atom(name) do
    case Process.whereis(name) do
      device when is_pid(device) ->
        async_io(device, output)

      nil ->
        raise "no device registered with the name #{inspect(name)}"
    end
  end

  defp async_io(device, output) when is_pid(device) do
    send(device, {:io_request, self(), make_ref(), {:put_chars, :unicode, output}})
  end

  defp format_event(level, msg, ts, md, state) do
    %{format: format, metadata: keys, colors: colors} = state

    format
    |> Logger.Formatter.format(level, msg, ts, take_metadata(md, keys))
    |> color_event(level, colors, md)
  end

  defp take_metadata(metadata, :all) do
    metadata
  end

  defp take_metadata(metadata, keys) do
    Enum.reduce(keys, [], fn key, acc ->
      case Keyword.fetch(metadata, key) do
        {:ok, val} -> [{key, val} | acc]
        :error -> acc
      end
    end)
  end

  defp color_event(data, _level, %{enabled: false}, _md), do: data

  defp color_event(data, level, %{enabled: true} = colors, md) do
    color = md[:ansi_color] || Map.fetch!(colors, level)
    IO.ANSI.format([color, data])
  end
end

defmodule Livebook.Evaluator.DefaultFormatter do
  @moduledoc false

  # The formatter used by Livebook for rendering the results.

  @behaviour Livebook.Evaluator.Formatter

  @impl true
  def format({:ok, :"do not show this result in output"}) do
    # Functions in the `IEx.Helpers` module return this specific value
    # to indicate no result should be printed in the iex shell,
    # so we respect that as well.
    :ignored
  end

  def format({:ok, {:module, _, _, _} = value}) do
    inspected = inspect(value, inspect_opts(limit: 10))
    {:inspect, inspected}
  end

  @compile {:no_warn_undefined, {VegaLite, :to_spec, 1}}

  def format({:ok, value}) do
    cond do
      is_struct(value, VegaLite) and function_exported?(VegaLite, :to_spec, 1) ->
        {:vega_lite_spec, VegaLite.to_spec(value)}

      true ->
        inspected = inspect(value, inspect_opts())
        {:inspect, inspected}
    end
  end

  def format({:error, kind, error, stacktrace}) do
    formatted = Exception.format(kind, error, stacktrace)
    {:error, formatted}
  end

  defp inspect_opts(opts \\ []) do
    default_opts = [pretty: true, width: 100, syntax_colors: syntax_colors()]
    Keyword.merge(default_opts, opts)
  end

  defp syntax_colors() do
    # Note: we intentionally don't specify colors
    # for `:binary`, `:list`, `:map` and `:tuple`
    # and rely on these using the default text color.
    # This way we avoid a bunch of HTML tags for coloring commas, etc.
    [
      atom: :blue,
      # binary: :light_black,
      boolean: :magenta,
      # list: :light_black,
      # map: :light_black,
      number: :blue,
      nil: :magenta,
      regex: :red,
      string: :green,
      # tuple: :light_black,
      reset: :reset
    ]
  end
end

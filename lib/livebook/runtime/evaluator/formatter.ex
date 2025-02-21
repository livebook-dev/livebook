defmodule Livebook.Runtime.Evaluator.Formatter do
  require Logger

  @compile {:no_warn_undefined, {Kino.Render, :to_livebook, 1}}
  @compile {:no_warn_undefined, {Pythonx, :eval, 2}}
  @compile {:no_warn_undefined, {Pythonx, :decode, 1}}

  @doc """
  Formats evaluation result into an output.

  We format the result into one of the standardized runtime outputs.
  Other than that, formatting is important to convert potentially
  large result into a more compact representation. Finally, we want
  to format in the runtime node, because it oftentimes relies on the
  `inspect` protocol implementations from external packages.
  """
  @spec format_result(
          Livebook.Runtime.language(),
          Livebook.Runtime.Evaluator.evaluation_result()
        ) :: Livebook.Runtime.output()
  def format_result(language, result)

  def format_result(:elixir, {:ok, :"do not show this result in output"}) do
    # Functions in the `IEx.Helpers` module return this specific value
    # to indicate no result should be printed in the iex shell,
    # so we respect that as well.
    %{type: :ignored}
  end

  def format_result(:elixir, {:ok, {:module, _, _, _} = value}) do
    to_inspect_output(value, limit: 10)
  end

  def format_result(:elixir, {:ok, value}) do
    to_output(value)
  end

  def format_result(:erlang, {:ok, value}) do
    erlang_to_output(value)
  end

  def format_result(:erlang, {:error, kind, error, stacktrace}) do
    if is_exception(error) do
      format_result(:elixir, {:error, kind, error, stacktrace})
    else
      formatted =
        :erl_error.format_exception(kind, error, stacktrace)
        |> error_color
        |> :erlang.list_to_binary()

      %{type: :error, message: formatted, context: error_context(error)}
    end
  end

  def format_result(:python, {:ok, nil}) do
    %{type: :ignored}
  end

  def format_result(:python, {:ok, value}) do
    repr_string = Pythonx.eval("repr(value)", %{"value" => value}) |> elem(0) |> Pythonx.decode()
    %{type: :terminal_text, text: repr_string, chunk: false}
  end

  def format_result(:python, {:error, _kind, error, _stacktrace})
      when is_struct(error, Pythonx.Error) do
    formatted =
      Pythonx.eval(
        """
        import traceback
        # For SyntaxErrors the traceback is not relevant
        traceback_ = None if isinstance(value, SyntaxError) else traceback_
        traceback.format_exception(type, value, traceback_)
        """,
        %{"type" => error.type, "value" => error.value, "traceback_" => error.traceback}
      )
      |> elem(0)
      |> Pythonx.decode()
      |> error_color()
      |> IO.iodata_to_binary()

    %{type: :error, message: formatted, context: nil}
  end

  def format_result(:"pyproject.toml", {:ok, _value}) do
    %{type: :terminal_text, text: "Ok", chunk: false}
  end

  def format_result(_language, {:error, kind, error, stacktrace}) do
    formatted = format_error(kind, error, stacktrace)
    %{type: :error, message: formatted, context: error_context(error)}
  end

  defp to_output(value) do
    # Kino is a "client side" extension for Livebook that may be
    # installed into the runtime node. If it is installed we use
    # its more precise output rendering rules.
    if Code.ensure_loaded?(Kino.Render) do
      try do
        Kino.Render.to_livebook(value)
      catch
        kind, error ->
          formatted = format_error(kind, error, __STACKTRACE__)
          Logger.error(formatted)
          to_inspect_output(value)
      end
    else
      to_inspect_output(value)
    end
  end

  defp to_inspect_output(value, opts \\ []) do
    try do
      inspected = inspect(value, inspect_opts(opts))
      %{type: :terminal_text, text: inspected, chunk: false}
    catch
      kind, error ->
        formatted = format_error(kind, error, __STACKTRACE__)
        %{type: :error, message: formatted, context: error_context(error)}
    end
  end

  def inspect_opts(opts \\ []) do
    default_opts = [pretty: true, width: 100, syntax_colors: syntax_colors()]
    Keyword.merge(default_opts, opts)
  end

  def syntax_colors() do
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

  defp format_error(kind, error, stacktrace) do
    {blamed, stacktrace} =
      case error do
        %CompileError{description: "cannot compile file (errors have been logged)" <> _, line: 0} ->
          {%CompileError{description: "cannot compile cell (errors have been logged)"}, []}

        _ ->
          Exception.blame(kind, error, stacktrace)
      end

    banner =
      case blamed do
        %FunctionClauseError{} ->
          banner = Exception.format_banner(kind, error, stacktrace)
          blame = FunctionClauseError.blame(blamed, &inspect(&1, inspect_opts()), &blame_match/1)
          [error_color(banner), pad(blame)]

        _ ->
          banner = Exception.format_banner(kind, blamed, stacktrace)
          error_color(banner)
      end

    message =
      if stacktrace == [] do
        banner
      else
        stacktrace = format_stacktrace(stacktrace)
        [banner, "\n", error_color(stacktrace)]
      end

    IO.iodata_to_binary(message)
  end

  defp format_stacktrace(trace) do
    case trace do
      [] -> "\n"
      _ -> "    " <> Enum.map_join(trace, "\n    ", &format_stacktrace_entry(&1)) <> "\n"
    end
  end

  @doc """
  Formats a stacktrace entry keeping only the #cell: bits.
  """
  def format_stacktrace_entry(entry) do
    entry =
      with {mod, fun, arity, info} <- entry,
           [_ | _] = file <- info[:file],
           [_, cell] <- :string.split(file, "#cell:") do
        {mod, fun, arity, Keyword.put(info, :file, ~c"#cell:" ++ cell)}
      else
        _ -> entry
      end

    Exception.format_stacktrace_entry(entry)
  end

  defp blame_match(%{match?: true, node: node}), do: Macro.to_string(node)

  defp blame_match(%{match?: false, node: node}) do
    node
    |> Macro.to_string()
    |> error_color()
    |> IO.iodata_to_binary()
  end

  defp pad(string) do
    "    " <> String.replace(string, "\n", "\n    ")
  end

  defp error_color(string) do
    IO.ANSI.format([:red, string], true)
  end

  defp error_context(%System.EnvError{env: "LB_" <> secret_name}),
    do: {:missing_secret, secret_name}

  defp error_context(error) when is_struct(error, Kino.InterruptError),
    do: {:interrupt, error.variant, error.message}

  defp error_context(error) when is_struct(error, Kino.FS.ForbiddenError),
    do: {:file_entry_forbidden, error.name}

  defp error_context(error) when is_struct(error, Mix.Error),
    do: :dependencies

  defp error_context(_), do: nil

  defp erlang_to_output(value) do
    text = :io_lib.format("~p", [value]) |> IO.iodata_to_binary()
    %{type: :terminal_text, text: text, chunk: false}
  end
end

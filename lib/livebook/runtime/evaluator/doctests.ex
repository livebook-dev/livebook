defmodule Livebook.Runtime.Evaluator.Doctests do
  @moduledoc false

  @test_timeout 5_000
  @line_width 80
  @pad_size 3

  @doc """
  Runs doctests in the given modules.
  """
  @spec run(list(module()), String.t()) :: :ok
  def run(modules, code) do
    doctests_specs =
      for module <- modules, doctests_spec = doctests_spec(module), do: doctests_spec

    do_run(doctests_specs, code)
  end

  defp doctests_spec(module) do
    case Code.fetch_docs(module) do
      {:docs_v1, _, _, _, doc_content, _, member_docs} ->
        funs =
          for {{:function, name, arity}, annotation, _signatures, _doc, _meta} <- member_docs,
              do: %{name: name, arity: arity, generated: :erl_anno.generated(annotation)}

        {generated_funs, regular_funs} = Enum.split_with(funs, & &1.generated)

        if regular_funs != [] or is_map(doc_content) do
          except = Enum.map(generated_funs, &{&1.name, &1.arity})
          %{module: module, except: except}
        end

      _ ->
        nil
    end
  end

  defp do_run([], _code), do: :ok

  defp do_run(doctests_specs, code) do
    case define_test_module(doctests_specs) do
      {:ok, test_module} ->
        lines = String.split(code, ["\r\n", "\n"])

        # Ignore test cases that don't actually point to a doctest
        # in the source code
        tests = Enum.filter(test_module.tests, &doctest_at_line?(lines, &1.tags.doctest_line))

        if tests != [] do
          tests
          |> Enum.sort_by(& &1.tags.doctest_line)
          |> Enum.each(fn test ->
            report_doctest_running(test)
            test = run_test(test)
            report_doctest_result(test, lines)
          end)
        end

        delete_test_module(test_module)

      {:error, kind, error} ->
        put_output({:error, colorize(:red, Exception.format(kind, error, [])), :other})
    end

    :ok
  end

  defp doctest_at_line?(lines, line_number) do
    if line = Enum.at(lines, line_number - 1) do
      case String.trim_leading(line) do
        "iex>" <> _ -> true
        "iex(" <> _ -> true
        _ -> false
      end
    else
      false
    end
  end

  defp report_doctest_running(test) do
    send_doctest_report(%{
      line: test.tags.doctest_line,
      status: :running
    })
  end

  defp report_doctest_result(%{state: nil} = test, _lines) do
    send_doctest_report(%{
      line: test.tags.doctest_line,
      status: :success
    })
  end

  defp report_doctest_result(%{state: {:failed, failure}} = test, lines) do
    doctest_line = test.tags.doctest_line
    [prompt_line | _] = lines = Enum.drop(lines, doctest_line - 1)

    end_line =
      if end_line = test.tags[:doctest_data][:end_line] do
        end_line
      else
        # TODO: Remove this branch once we require Elixir v1.15+
        interval =
          lines
          |> Enum.take_while(&(not end_of_doctest?(&1)))
          |> length()

        interval + doctest_line - 1
      end

    end_line =
      with {:error, %ExUnit.AssertionError{}, [{_, _, _, location} | _]} <- failure,
           assertion_line = location[:line],
           # TODO: Remove this check once we require Elixir v1.15+
           true <- is_integer(test.tags[:doctest_data][:end_line]),
           true <- assertion_line in doctest_line..end_line do
        end_line -
          length(
            lines
            |> Enum.take(end_line - doctest_line + 1)
            |> Enum.drop(assertion_line - doctest_line)
            |> Enum.drop_while(&prompt?(&1))
            |> Enum.drop_while(&(not prompt?(&1)))
          )
      else
        _ ->
          end_line
      end

    send_doctest_report(%{
      column: count_columns(prompt_line, 0),
      line: doctest_line,
      end_line: end_line,
      status: :failed,
      details: IO.iodata_to_binary(format_failure(failure, test))
    })
  end

  defp count_columns(" " <> rest, counter), do: count_columns(rest, counter + 1)
  defp count_columns("\t" <> rest, counter), do: count_columns(rest, counter + 2)
  defp count_columns(_, counter), do: counter

  defp prompt?(line) do
    case String.trim_leading(line) do
      "iex>" <> _ -> true
      "iex(" <> _ -> true
      "...>" <> _ -> true
      "...(" <> _ -> true
      _ -> false
    end
  end

  defp end_of_doctest?(line) do
    case String.trim_leading(line) do
      "" -> true
      "```" <> _ -> true
      "~~~" <> _ -> true
      "'''" <> _ -> true
      "\"\"\"" <> _ -> true
      _ -> false
    end
  end

  defp define_test_module(doctests_specs) do
    id =
      doctests_specs
      |> Enum.map(& &1.module)
      |> Enum.sort()
      |> Enum.map_join("-", fn module ->
        module
        |> Atom.to_string()
        |> String.replace_prefix("Elixir.", "")
      end)
      |> :erlang.md5()
      |> Base.encode32(padding: false)

    name = Module.concat([LivebookDoctest, "TestModule#{id}"])

    try do
      defmodule name do
        use ExUnit.Case, register: false

        for doctests_spec <- doctests_specs do
          doctest doctests_spec.module, except: doctests_spec.except
        end
      end

      {:ok, name.__ex_unit__()}
    catch
      kind, error -> {:error, kind, error}
    end
  end

  defp delete_test_module(%{name: module}) do
    :code.delete(module)
    :code.purge(module)
  end

  defp run_test(test) do
    runner_pid = self()

    {test_pid, test_ref} =
      spawn_monitor(fn ->
        test = exec_test(test)
        send(runner_pid, {:test_finished, self(), test})
      end)

    receive_test_reply(test, test_pid, test_ref)
  end

  defp receive_test_reply(test, test_pid, test_ref) do
    receive do
      {:test_finished, ^test_pid, test} ->
        Process.demonitor(test_ref, [:flush])
        test

      {:DOWN, ^test_ref, :process, ^test_pid, error} ->
        %{test | state: failed({:EXIT, test_pid}, error, [])}
    after
      @test_timeout ->
        case Process.info(test_pid, :current_stacktrace) do
          {:current_stacktrace, stacktrace} ->
            Process.exit(test_pid, :kill)

            receive do
              {:DOWN, ^test_ref, :process, ^test_pid, _} -> :ok
            end

            exception = RuntimeError.exception("doctest timed out after #{@test_timeout} ms")
            %{test | state: failed(:error, exception, prune_stacktrace(stacktrace))}

          nil ->
            receive_test_reply(test, test_pid, test_ref)
        end
    end
  end

  defp exec_test(%ExUnit.Test{module: module, name: name} = test) do
    apply(module, name, [:none])
    test
  catch
    kind, error ->
      %{test | state: failed(kind, error, prune_stacktrace(__STACKTRACE__))}
  end

  defp failed(kind, reason, stack) do
    {:failed, {kind, Exception.normalize(kind, reason, stack), stack}}
  end

  # As soon as we see our runner module, we ignore the rest of the stacktrace
  def prune_stacktrace([{__MODULE__, _, _, _} | _]), do: []
  def prune_stacktrace([h | t]), do: [h | prune_stacktrace(t)]
  def prune_stacktrace([]), do: []

  # Formatting

  defp format_failure({:error, %ExUnit.AssertionError{} = reason, _stack}, _test) do
    diff =
      ExUnit.Formatter.format_assertion_diff(
        reason,
        @pad_size + 2,
        @line_width,
        &diff_formatter/2
      )

    expected = diff[:right]
    got = diff[:left]

    {expected_label, got_label} =
      if reason.doctest == ExUnit.AssertionError.no_value() do
        {"right", "left"}
      else
        {"expected", "got"}
      end

    message_io =
      if_io(reason.message != "Doctest failed", fn ->
        message = String.replace_prefix(reason.message, "Doctest failed: ", "")
        colorize(:red, message)
      end)

    expected_io =
      if_io(expected, fn ->
        [format_label(expected_label), "\n  ", expected]
      end)

    got_io =
      if_io(got, fn ->
        [format_label(got_label), "\n  ", got]
      end)

    [message_io, expected_io, got_io]
    |> Enum.filter(&(&1 != []))
    |> Enum.intersperse("\n")
  end

  defp format_failure({kind, reason, stacktrace}, test) do
    {blamed, stacktrace} = Exception.blame(kind, reason, stacktrace)

    banner =
      case blamed do
        %FunctionClauseError{} ->
          banner = Exception.format_banner(kind, reason, stacktrace)
          inspect_opts = Livebook.Runtime.Evaluator.Formatter.inspect_opts()
          blame = FunctionClauseError.blame(blamed, &inspect(&1, inspect_opts), &blame_match/1)
          colorize(:red, banner) <> blame

        _ ->
          banner = Exception.format_banner(kind, blamed, stacktrace)
          colorize(:red, banner)
      end

    if stacktrace == [] do
      banner
    else
      [
        banner,
        "\n",
        format_label("stacktrace"),
        format_stacktrace(stacktrace, test.module, test.name)
      ]
    end
  end

  defp if_io(value, fun), do: if(value, do: fun.(), else: [])

  defp format_stacktrace(stacktrace, test_case, test) do
    for entry <- stacktrace do
      message = format_stacktrace_entry(entry, test_case, test)
      "\n" <> pad(message, 2)
    end
  end

  defp format_stacktrace_entry({test_case, test, _, location}, test_case, test) do
    "doctest at line #{location[:line]}"
  end

  defp format_stacktrace_entry(entry, _test_case, _test) do
    Exception.format_stacktrace_entry(entry)
  end

  defp format_label(label), do: colorize(:cyan, "#{label}:")

  defp pad(string, pad_size) do
    padding = String.duplicate(" ", pad_size)
    padding <> String.replace(string, "\n", "\n" <> padding)
  end

  defp blame_match(%{match?: true, node: node}), do: Macro.to_string(node)

  defp blame_match(%{match?: false, node: node}) do
    colorize(:red, Macro.to_string(node))
  end

  defp diff_formatter(:diff_enabled?, _doc), do: true

  defp diff_formatter(key, doc) do
    colors = [
      diff_delete: :red,
      diff_delete_whitespace: IO.ANSI.color_background(4, 0, 0),
      diff_insert: :green,
      diff_insert_whitespace: IO.ANSI.color_background(0, 3, 0)
    ]

    Inspect.Algebra.color(doc, key, %Inspect.Opts{syntax_colors: colors})
  end

  defp colorize(color, string) do
    [color, string, :reset]
    |> IO.ANSI.format_fragment(true)
    |> IO.iodata_to_binary()
  end

  defp put_output(output) do
    send_livebook_message({:livebook_put_output, output})
  end

  defp send_doctest_report(doctest_report) do
    send_livebook_message({:livebook_doctest_report, doctest_report})
  end

  defp send_livebook_message(message) do
    gl = Process.group_leader()
    ref = make_ref()

    send(gl, {:io_request, self(), ref, message})

    receive do
      {:io_reply, ^ref, reply} -> {:ok, reply}
    end
  end
end

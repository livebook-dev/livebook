defmodule Livebook.TestHelpers do
  import Phoenix.LiveViewTest
  import ExUnit.Assertions

  alias Livebook.Session.Data

  @doc """
  Creates file structure according to the given specification.
  """
  def create_tree!(path, items) do
    for {name, content} <- items do
      child_path = Path.join(path, to_string(name))

      case content do
        items when is_list(items) ->
          File.mkdir!(child_path)
          create_tree!(child_path, items)

        content when is_binary(content) ->
          File.write!(child_path, content)
      end
    end
  end

  @doc """
  Creates the given number of subdirectories and returns their paths.
  """
  def create_subdirs!(path, number) do
    for n <- 1..number//1 do
      child_path = Path.join(path, "subdir#{n}")
      File.mkdir_p!(child_path)
      child_path
    end
  end

  @doc """
  Applies the given list of operations to `Livebook.Session.Data`.

  Raises if any of the operations results in an error.
  """
  def data_after_operations!(data \\ Data.new(), operations) do
    operations
    |> List.flatten()
    |> Enum.reduce(data, fn operation, data ->
      case Data.apply_operation(data, operation) do
        {:ok, data, _action} ->
          data

        :error ->
          raise "failed to set up test data, operation #{inspect(operation)} returned an error"
      end
    end)
  end

  @doc """
  Converts a Unix-like absolute path into OS-compatible absolute path.
  """
  defmacro p("/" <> path), do: Path.expand("/") <> path

  @doc """
  Confirms the action guarded by `LivebookWeb.Confirm/3` and
  returns the rendered result.
  """
  def render_confirm(view, options \\ %{}) do
    options =
      for {option, value} <- options,
          into: %{},
          do: {Atom.to_string(option), Atom.to_string(value)}

    view
    |> element(~s/[data-el-confirm-form]/)
    |> render_submit(%{"options" => options})
  end

  @doc """
  Builds code that renders the given output as part of evaluation.
  """
  def source_for_output(output) do
    quote do
      send(
        Process.group_leader(),
        {:io_request, self(), make_ref(), {:livebook_put_output, unquote(Macro.escape(output))}}
      )
    end
    |> Macro.to_string()
  end

  @doc """
  Builds code that renders the given output as part of evaluation.
  """
  def source_for_input_read(input_id) do
    quote do
      send(
        Process.group_leader(),
        {:io_request, self(), make_ref(), {:livebook_get_input_value, unquote(input_id)}}
      )
    end
    |> Macro.to_string()
  end

  @doc """
  Builds code that awaits for a messages before finishing.

  Returns `{code, continue_fun}`, where calling `continue_fun` should
  continue execution. Embedded runtime must be used for this to work.
  """
  def source_for_blocking() do
    name = Livebook.Utils.random_short_id() |> String.to_atom()

    Process.register(self(), name)

    code =
      quote do
        # We assume the test uses the Embedded runtime, so we can
        # target the process by name
        send(unquote(name), {:started, self()})
        receive do: (:finish -> :ok)
      end
      |> Macro.to_string()

    continue_fun = fn ->
      assert_receive {:started, pid}
      send(pid, :finish)
    end

    {code, continue_fun}
  end

  @doc """
  Builds a terminal output map.
  """
  defmacro terminal_text(text, chunk \\ false) do
    quote do
      %{type: :terminal_text, text: unquote(text), chunk: unquote(chunk)}
    end
  end

  def clean_message(message) do
    message
    |> remove_trailing_whitespace()
    |> remove_ansi()
  end

  defp remove_trailing_whitespace(string) do
    String.replace(string, ~r/ +$/m, "")
  end

  defp remove_ansi(string) do
    String.replace(string, ~r/\e\[\d+m/, "")
  end
end

defmodule Livebook.Intellisense.DocsTest do
  use ExUnit.Case, async: true

  alias Livebook.Intellisense.Docs

  @code ~S'''
  defmodule GoToDefinition do
    @type t :: term()
    @type foo :: foo(:bar)
    @type foo(var) :: {var, t()}

    defmacro with_logging(do: block) do
      quote do
        require Logger
        Logger.debug("Running code")
        result = unquote(block)
        Logger.debug("Result: #{inspect(result)}")
        result
      end
    end

    @spec hello(var :: term()) :: foo(term())
    def hello(message) do
      {:bar, message}
    end
  end
  '''

  @tag :tmp_dir
  test "locate_definition/2 returns the definition's location with cell id and line",
       %{tmp_dir: tmp_dir} do
    Code.put_compiler_option(:debug_info, true)

    path = Path.join(tmp_dir, "Elixir.GoToDefinition.beam")
    [{module, bytecode}] = Code.compile_string(@code)
    File.write!(path, bytecode)
    path = to_charlist(path)

    assert Docs.locate_definition(path, {:module, module}) == {:ok, 1}
    assert Docs.locate_definition(path, {:function, :with_logging, 1}) == {:ok, 6}
    assert Docs.locate_definition(path, {:function, :hello, 1}) == {:ok, 17}
    assert Docs.locate_definition(path, {:type, :t, 0}) == {:ok, 2}
    assert Docs.locate_definition(path, {:type, :foo, 0}) == {:ok, 3}
    assert Docs.locate_definition(path, {:type, :foo, 1}) == {:ok, 4}
  after
    Code.put_compiler_option(:debug_info, false)
  end
end

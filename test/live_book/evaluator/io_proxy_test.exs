defmodule LiveBook.Evaluator.IOProxyTest do
  use ExUnit.Case, async: true

  alias LiveBook.Evaluator.IOProxy

  setup do
    {:ok, io} = IOProxy.start_link()
    IOProxy.configure(io, self(), :ref)
    %{io: io}
  end

  # Test the basic ways users interact with :stdio

  test "IO.puts", %{io: io} do
    IO.puts(io, "hey")
    assert_received {:evaluator_stdout, :ref, "hey\n"}
  end

  test "IO.write", %{io: io} do
    IO.write(io, "hey")
    assert_received {:evaluator_stdout, :ref, "hey"}
  end

  test "IO.inspect", %{io: io} do
    IO.inspect(io, %{}, [])
    assert_received {:evaluator_stdout, :ref, "%{}\n"}
  end

  test "IO.read", %{io: io} do
    assert IO.read(io, :all) == {:error, :enotsup}
  end

  test "IO.gets", %{io: io} do
    assert IO.gets(io, "> ") == {:error, :enotsup}
  end
end

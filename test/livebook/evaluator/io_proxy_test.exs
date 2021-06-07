defmodule Livebook.Evaluator.IOProxyTest do
  use ExUnit.Case, async: true

  alias Livebook.Evaluator.IOProxy

  setup do
    {:ok, io} = IOProxy.start_link()
    IOProxy.configure(io, self(), :ref)
    %{io: io}
  end

  describe ":stdio interoperability" do
    test "IO.puts", %{io: io} do
      IO.puts(io, "hey")
      assert_receive {:evaluation_output, :ref, "hey\n"}
    end

    test "IO.write", %{io: io} do
      IO.write(io, "hey")
      assert_receive {:evaluation_output, :ref, "hey"}
    end

    test "IO.inspect", %{io: io} do
      IO.inspect(io, %{}, [])
      assert_receive {:evaluation_output, :ref, "%{}\n"}
    end

    test "IO.read", %{io: io} do
      pid =
        spawn(fn ->
          receive do
            {:evaluation_input, :ref, ^io, ""} ->
              send(io, {:evaluation_input_reply, :error})
          end
        end)

      IOProxy.configure(io, pid, :ref)

      assert IO.read(io, :all) == ""
    end

    test "IO.gets", %{io: io} do
      pid =
        spawn(fn ->
          receive do
            {:evaluation_input, :ref, ^io, "name: "} ->
              send(io, {:evaluation_input_reply, {:ok, "Jake Peralta"}})
          end
        end)

      IOProxy.configure(io, pid, :ref)

      assert IO.gets(io, "name: ") == "Jake Peralta"
    end

    test "IO.gets :eof", %{io: io} do
      pid =
        spawn(fn ->
          receive do
            {:evaluation_input, :ref, ^io, "name: "} ->
              send(io, {:evaluation_input_reply, :error})
          end
        end)

      IOProxy.configure(io, pid, :ref)

      assert IO.gets(io, "name: ") == :eof
    end
  end

  test "buffers rapid output", %{io: io} do
    IO.puts(io, "hey")
    IO.puts(io, "hey")
    assert_receive {:evaluation_output, :ref, "hey\nhey\n"}
  end

  test "respects CR as line cleaner", %{io: io} do
    IO.write(io, "hey")
    IO.write(io, "\roverride\r")
    assert_receive {:evaluation_output, :ref, "\roverride\r"}
  end

  test "flush/1 synchronously sends buffer contents", %{io: io} do
    IO.puts(io, "hey")
    IOProxy.flush(io)
    assert_received {:evaluation_output, :ref, "hey\n"}
  end

  test "supports direct livebook output forwarding", %{io: io} do
    ref = make_ref()
    send(io, {:io_request, self(), ref, {:livebook_put_output, {:text, "[1, 2, 3]"}}})
    assert_receive {:io_reply, ^ref, :ok}

    assert_received {:evaluation_output, :ref, {:text, "[1, 2, 3]"}}
  end
end

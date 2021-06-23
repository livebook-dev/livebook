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
        spawn_link(fn ->
          reply_to_input_request(:ref, "", :error, 1)
        end)

      IOProxy.configure(io, pid, :ref)

      assert IO.read(io, :all) == {:error, "no matching Livebook input found"}
    end

    test "IO.gets", %{io: io} do
      pid =
        spawn_link(fn ->
          reply_to_input_request(:ref, "name: ", {:ok, "Jake Peralta"}, 1)
        end)

      IOProxy.configure(io, pid, :ref)

      assert IO.gets(io, "name: ") == "Jake Peralta"
    end

    test "IO.gets with no matching input", %{io: io} do
      pid =
        spawn_link(fn ->
          reply_to_input_request(:ref, "name: ", :error, 1)
        end)

      IOProxy.configure(io, pid, :ref)

      assert IO.gets(io, "name: ") == {:error, "no matching Livebook input found"}
    end
  end

  test "consumes the given input only once", %{io: io} do
    pid =
      spawn_link(fn ->
        reply_to_input_request(:ref, "name: ", {:ok, "Jake Peralta\nAmy Santiago\n"}, 1)
      end)

    IOProxy.configure(io, pid, :ref)

    assert IO.gets(io, "name: ") == "Jake Peralta\n"
    assert IO.gets(io, "name: ") == "Amy Santiago\n"
    assert IO.gets(io, "name: ") == :eof
  end

  test "clear_input_buffers/1 clears all buffered input information", %{io: io} do
    pid =
      spawn_link(fn ->
        reply_to_input_request(:ref, "name: ", {:ok, "Jake Peralta"}, 2)
      end)

    IOProxy.configure(io, pid, :ref)

    assert IO.gets(io, "name: ") == "Jake Peralta"
    IOProxy.clear_input_buffers(io)
    assert IO.gets(io, "name: ") == "Jake Peralta"
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
    put_livebook_output(io, {:text, "[1, 2, 3]"})

    assert_received {:evaluation_output, :ref, {:text, "[1, 2, 3]"}}
  end

  test "flush_widgets/1 returns new widget pids", %{io: io} do
    widget1_pid = IEx.Helpers.pid(0, 0, 0)
    widget2_pid = IEx.Helpers.pid(0, 0, 1)

    put_livebook_output(io, {:vega_lite_dynamic, widget1_pid})
    put_livebook_output(io, {:vega_lite_dynamic, widget2_pid})
    put_livebook_output(io, {:vega_lite_dynamic, widget1_pid})

    assert IOProxy.flush_widgets(io) == MapSet.new([widget1_pid, widget2_pid])
    assert IOProxy.flush_widgets(io) == MapSet.new()
  end

  test "getn/1 return first character", %{io: io} do
    pid =
      spawn_link(fn ->
        reply_to_input_request(:ref, "name: ", {:ok, "🐈 test\n"}, 1)
      end)

    IOProxy.configure(io, pid, :ref)

    assert IO.getn(io, "name: ") == "🐈"
  end

  test "getn/2 returns the number of defined characters ", %{io: io} do
    pid =
      spawn_link(fn ->
        reply_to_input_request(:ref, "name: ", {:ok, "Jake Peralta\nAmy Santiago\n"}, 1)
      end)

    IOProxy.configure(io, pid, :ref)

    assert IO.getn(io, "name: ", 13) == "Jake Peralta\n"
    assert IO.getn(io, "name: ", 13) == "Amy Santiago\n"
    assert IO.getn(io, "name: ", 13) == :eof
  end

  test "getn/2 all characters", %{io: io} do
    pid =
      spawn_link(fn ->
        reply_to_input_request(:ref, "name: ", {:ok, "Jake Peralta\nAmy Santiago\n"}, 1)
      end)

    IOProxy.configure(io, pid, :ref)

    assert IO.getn(io, "name: ", 10_000) == "Jake Peralta\nAmy Santiago\n"
  end

  # Helpers

  defp reply_to_input_request(_ref, _prompt, _reply, 0), do: :ok

  defp reply_to_input_request(ref, prompt, reply, times) do
    receive do
      {:evaluation_input, ^ref, reply_to, ^prompt} ->
        send(reply_to, {:evaluation_input_reply, reply})
        reply_to_input_request(ref, prompt, reply, times - 1)
    end
  end

  defp put_livebook_output(io, output) do
    ref = make_ref()
    send(io, {:io_request, self(), ref, {:livebook_put_output, output}})
    assert_receive {:io_reply, ^ref, :ok}
  end
end

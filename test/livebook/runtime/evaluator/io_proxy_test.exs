defmodule Livebook.Runtime.Evaluator.IOProxyTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime.Evaluator
  alias Livebook.Runtime.Evaluator.IOProxy

  setup do
    {:ok, object_tracker} = start_supervised(Evaluator.ObjectTracker)

    {:ok, _pid, evaluator} =
      start_supervised({Evaluator, [send_to: self(), object_tracker: object_tracker]})

    io = Process.info(evaluator.pid)[:group_leader]
    IOProxy.before_evaluation(io, :ref, "cell")
    %{io: io}
  end

  describe ":stdio interoperability" do
    test "IO.puts", %{io: io} do
      IO.puts(io, "hey")
      assert_receive {:runtime_evaluation_output, :ref, {:stdout, "hey\n"}}
    end

    test "IO.write", %{io: io} do
      IO.write(io, "hey")
      assert_receive {:runtime_evaluation_output, :ref, {:stdout, "hey"}}
    end

    test "IO.inspect", %{io: io} do
      IO.inspect(io, %{}, [])
      assert_receive {:runtime_evaluation_output, :ref, {:stdout, "%{}\n"}}
    end

    test "IO.read", %{io: io} do
      assert IO.read(io, :all) == {:error, :enotsup}
    end

    test "IO.gets", %{io: io} do
      assert IO.gets(io, "name: ") == {:error, :enotsup}
    end
  end

  describe "input" do
    test "responds to Livebook input request", %{io: io} do
      pid =
        spawn(fn ->
          assert livebook_get_input_value(io, "input1") == {:ok, :value}
        end)

      reply_to_input_request(:ref, "input1", {:ok, :value}, 1)

      await_termination(pid)
    end

    test "responds to subsequent requests with the same value", %{io: io} do
      pid =
        spawn(fn ->
          assert livebook_get_input_value(io, "input1") == {:ok, :value}
          assert livebook_get_input_value(io, "input1") == {:ok, :value}
        end)

      reply_to_input_request(:ref, "input1", {:ok, :value}, 1)

      await_termination(pid)
    end

    test "before_evaluation/3 clears all cached input information", %{io: io} do
      pid =
        spawn_link(fn ->
          IOProxy.before_evaluation(io, :ref, "cell")
          assert livebook_get_input_value(io, "input1") == {:ok, :value1}
          IOProxy.before_evaluation(io, :ref, "cell")
          assert livebook_get_input_value(io, "input1") == {:ok, :value2}
        end)

      reply_to_input_request(:ref, "input1", {:ok, :value1}, 1)
      reply_to_input_request(:ref, "input1", {:ok, :value2}, 1)

      await_termination(pid)
    end
  end

  test "buffers rapid output", %{io: io} do
    IO.puts(io, "hey")
    IO.puts(io, "hey")
    assert_receive {:runtime_evaluation_output, :ref, {:stdout, "hey\nhey\n"}}
  end

  test "respects CR as line cleaner", %{io: io} do
    IO.write(io, "hey")
    IO.write(io, "\roverride\r")
    assert_receive {:runtime_evaluation_output, :ref, {:stdout, "\roverride\r"}}
  end

  test "after_evaluation/1 synchronously sends buffer contents", %{io: io} do
    IO.puts(io, "hey")
    IOProxy.after_evaluation(io)
    assert_received {:runtime_evaluation_output, :ref, {:stdout, "hey\n"}}
  end

  test "supports direct livebook output forwarding", %{io: io} do
    livebook_put_output(io, {:text, "[1, 2, 3]"})

    assert_received {:runtime_evaluation_output, :ref, {:text, "[1, 2, 3]"}}
  end

  test "supports direct livebook output forwarding for a specific client", %{io: io} do
    livebook_put_output_to(io, "client1", {:text, "[1, 2, 3]"})

    assert_received {:runtime_evaluation_output_to, "client1", :ref, {:text, "[1, 2, 3]"}}
  end

  describe "token requests" do
    test "returns different tokens for subsequent calls", %{io: io} do
      IOProxy.before_evaluation(io, :ref1, "cell1")
      token1 = livebook_generate_token(io)
      token2 = livebook_generate_token(io)
      assert token1 != token2
    end

    test "returns different tokens for different refs", %{io: io} do
      IOProxy.before_evaluation(io, :ref1, "cell1")
      token1 = livebook_generate_token(io)
      IOProxy.before_evaluation(io, :ref2, "cell2")
      token2 = livebook_generate_token(io)
      assert token1 != token2
    end

    test "returns same tokens for the same ref", %{io: io} do
      IOProxy.before_evaluation(io, :ref, "cell")
      token1 = livebook_generate_token(io)
      token2 = livebook_generate_token(io)
      IOProxy.before_evaluation(io, :ref, "cell")
      token3 = livebook_generate_token(io)
      token4 = livebook_generate_token(io)
      assert token1 == token3
      assert token2 == token4
    end
  end

  describe "evaluation file requests" do
    test "returns the before_evaluation file", %{io: io} do
      IOProxy.before_evaluation(io, :ref1, "cell1")
      assert livebook_get_evaluation_file(io) == "cell1"
    end
  end

  # Helpers

  defp reply_to_input_request(_ref, _input_id, _reply, 0), do: :ok

  defp reply_to_input_request(ref, input_id, reply, times) do
    receive do
      {:runtime_evaluation_input_request, ^ref, reply_to, ^input_id} ->
        send(reply_to, {:runtime_evaluation_input_reply, reply})
        reply_to_input_request(ref, input_id, reply, times - 1)
    end
  end

  defp livebook_put_output(io, output) do
    io_request(io, {:livebook_put_output, output})
  end

  defp livebook_put_output_to(io, client_id, output) do
    io_request(io, {:livebook_put_output_to, client_id, output})
  end

  defp livebook_get_input_value(io, input_id) do
    io_request(io, {:livebook_get_input_value, input_id})
  end

  defp livebook_generate_token(io) do
    io_request(io, :livebook_generate_token)
  end

  defp livebook_get_evaluation_file(io) do
    io_request(io, :livebook_get_evaluation_file)
  end

  defp io_request(io, request) do
    ref = make_ref()
    send(io, {:io_request, self(), ref, request})
    assert_receive {:io_reply, ^ref, reply}
    reply
  end

  defp await_termination(pid) do
    ref = Process.monitor(pid)
    assert_receive {:DOWN, ^ref, :process, _, _}
  end
end

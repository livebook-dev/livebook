defmodule Livebook.Runtime.ErlDist.SmartCellGLTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime.ErlDist.SmartCellGL

  test "forwards regular IO requests to its group leader" do
    assert ExUnit.CaptureIO.capture_io(:stdio, fn ->
             runtime_broadcast_to = self()
             pid = start_supervised!({SmartCellGL, runtime_broadcast_to})

             IO.puts(pid, "hey")
           end) == "hey\n"
  end

  test "supports :livebook_get_broadcast_target request" do
    runtime_broadcast_to = self()
    pid = start_supervised!({SmartCellGL, runtime_broadcast_to})

    assert {:ok, ^runtime_broadcast_to} = io_request(pid, :livebook_get_broadcast_target)
  end

  defp io_request(io, request) do
    ref = make_ref()
    send(io, {:io_request, self(), ref, request})
    assert_receive {:io_reply, ^ref, reply}
    reply
  end
end

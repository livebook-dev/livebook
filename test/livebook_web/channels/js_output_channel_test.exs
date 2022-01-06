defmodule LivebookWeb.JSOutputChannelTest do
  use LivebookWeb.ChannelCase

  setup do
    {:ok, _, socket} =
      LivebookWeb.Socket
      |> socket()
      |> subscribe_and_join(LivebookWeb.JSOutputChannel, "js_output")

    %{socket: socket}
  end

  test "loads initial data from the widget server and pushes to the client", %{socket: socket} do
    push(socket, "connect", %{"session_token" => session_token(), "ref" => "1"})

    assert_receive {:connect, from, %{}}
    send(from, {:connect_reply, [1, 2, 3], %{ref: "1"}})

    assert_push "init:1", %{"data" => [1, 2, 3]}
  end

  test "sends events received from widget server to the client", %{socket: socket} do
    send(socket.channel_pid, {:event, "ping", [1, 2, 3], %{ref: "1"}})

    assert_push "event:1", %{"event" => "ping", "payload" => [1, 2, 3]}
  end

  test "sends client events to the corresponding widget server", %{socket: socket} do
    push(socket, "connect", %{"session_token" => session_token(), "ref" => "1"})

    assert_receive {:connect, from, %{}}
    send(from, {:connect_reply, [1, 2, 3], %{ref: "1"}})

    push(socket, "event", %{"event" => "ping", "payload" => [1, 2, 3], "ref" => "1"})

    assert_receive {:event, "ping", [1, 2, 3], %{origin: _origin}}
  end

  defp session_token() do
    Phoenix.Token.sign(LivebookWeb.Endpoint, "js output", %{pid: self()})
  end
end

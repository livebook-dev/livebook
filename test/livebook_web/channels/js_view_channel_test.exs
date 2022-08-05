defmodule LivebookWeb.JSViewChannelTest do
  use LivebookWeb.ChannelCase, async: true

  setup do
    session_id = Livebook.Utils.random_node_aware_id()

    {:ok, _, socket} =
      LivebookWeb.Socket
      |> socket()
      |> subscribe_and_join(LivebookWeb.JSViewChannel, "js_view", %{
        "session_id" => session_id,
        "client_id" => Livebook.Utils.random_id()
      })

    %{socket: socket}
  end

  test "loads initial data from the widget server and pushes to the client", %{socket: socket} do
    push(socket, "connect", %{"session_token" => session_token(), "ref" => "1", "id" => "id1"})

    assert_receive {:connect, from, %{}}
    send(from, {:connect_reply, [1, 2, 3], %{ref: "1"}})

    assert_push "init:1:id1", %{"root" => [nil, [1, 2, 3]]}
  end

  test "loads initial data for multiple connections separately", %{socket: socket} do
    push(socket, "connect", %{"session_token" => session_token(), "ref" => "1", "id" => "id1"})
    push(socket, "connect", %{"session_token" => session_token(), "ref" => "1", "id" => "id2"})

    assert_receive {:connect, from, %{}}
    send(from, {:connect_reply, [1, 2, 3], %{ref: "1"}})
    assert_push "init:1:id1", %{"root" => [nil, [1, 2, 3]]}

    assert_receive {:connect, from, %{}}
    send(from, {:connect_reply, [1, 2, 3], %{ref: "1"}})
    assert_push "init:1:id2", %{"root" => [nil, [1, 2, 3]]}
  end

  test "sends client events to the corresponding widget server", %{socket: socket} do
    push(socket, "connect", %{"session_token" => session_token(), "ref" => "1", "id" => "id1"})

    assert_receive {:connect, from, %{}}
    send(from, {:connect_reply, [1, 2, 3], %{ref: "1"}})

    push(socket, "event", %{"root" => [["ping", "1"], [1, 2, 3]]})

    assert_receive {:event, "ping", [1, 2, 3], %{origin: _origin}}
  end

  test "sends server events to the target client", %{socket: socket} do
    push(socket, "connect", %{"session_token" => session_token(), "ref" => "1", "id" => "id1"})

    assert_receive {:connect, from, %{}}
    send(from, {:connect_reply, [1, 2, 3], %{ref: "1"}})

    send(from, {:event, "ping", [1, 2, 3], %{ref: "1"}})
    assert_push "event:1", %{"root" => [["ping"], [1, 2, 3]]}
  end

  describe "binary payload" do
    test "initial data", %{socket: socket} do
      push(socket, "connect", %{"session_token" => session_token(), "ref" => "1", "id" => "id1"})

      assert_receive {:connect, from, %{}}
      payload = {:binary, %{message: "hey"}, <<1, 2, 3>>}
      send(from, {:connect_reply, payload, %{ref: "1"}})

      assert_push "init:1:id1",
                  {:binary, <<24::size(32), "[null,{\"message\":\"hey\"}]", 1, 2, 3>>}
    end

    test "form client to server", %{socket: socket} do
      push(socket, "connect", %{"session_token" => session_token(), "ref" => "1", "id" => "id1"})

      assert_receive {:connect, from, %{}}
      send(from, {:connect_reply, [1, 2, 3], %{ref: "1"}})

      raw = {:binary, <<32::size(32), "[[\"ping\",\"1\"],{\"message\":\"hey\"}]", 1, 2, 3>>}
      push(socket, "event", raw)

      payload = {:binary, %{"message" => "hey"}, <<1, 2, 3>>}
      assert_receive {:event, "ping", ^payload, %{origin: _origin}}
    end
  end

  defp session_token() do
    Phoenix.Token.sign(LivebookWeb.Endpoint, "js view", %{pid: self()})
  end
end

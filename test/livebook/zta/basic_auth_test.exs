defmodule Livebook.ZTA.BasicAuthTest do
  use ExUnit.Case, async: true
  use Plug.Test

  alias Livebook.ZTA.BasicAuth

  import Plug.BasicAuth, only: [encode_basic_auth: 2]

  @name Context.Test.BasicAuth

  setup do
    username = "ChonkierCat"
    password = Livebook.Utils.random_long_id()
    options = [name: @name, identity_key: "#{username}:#{password}"]

    {:ok, username: username, password: password, options: options, conn: conn(:get, "/")}
  end

  test "returns the user_identity when credentials are valid", context do
    authorization = encode_basic_auth(context.username, context.password)
    conn = put_req_header(context.conn, "authorization", authorization)
    start_supervised!({BasicAuth, context.options})

    assert {_conn, %{}} = BasicAuth.authenticate(@name, conn, [])
  end

  test "returns nil when the username is invalid", context do
    authorization = encode_basic_auth("foo", context.password)
    conn = put_req_header(context.conn, "authorization", authorization)
    start_supervised!({BasicAuth, context.options})

    assert {_conn, nil} = BasicAuth.authenticate(@name, conn, [])
  end

  test "returns nil when the password is invalid", context do
    authorization = encode_basic_auth(context.username, Livebook.Utils.random_long_id())
    conn = put_req_header(context.conn, "authorization", authorization)
    start_supervised!({BasicAuth, context.options})

    assert {_conn, nil} = BasicAuth.authenticate(@name, conn, [])
  end
end

defmodule Livebook.Runtime.FlyTest do
  use ExUnit.Case, async: true

  # To run these tests create a Fly app, generate deployment token,
  # then set TEST_FLY_APP_NAME and TEST_FLY_API_TOKEN
  @moduletag :fly

  alias Livebook.Runtime

  @assert_receive_timeout 10_000

  setup do
    Livebook.FlyAPI.passthrough()
    :ok
  end

  test "connecting flow" do
    fly = fly!()
    config = config(%{token: fly.token, app_name: fly.app_name})

    assert [] = fly_run(fly, ~w(machine list))

    pid = Runtime.Fly.new(config) |> Runtime.connect()

    Req.Test.allow(Livebook.FlyAPI, self(), pid)

    assert_receive {:runtime_connect_info, ^pid, "create machine"}, @assert_receive_timeout
    assert_receive {:runtime_connect_info, ^pid, "start proxy"}, @assert_receive_timeout
    assert_receive {:runtime_connect_info, ^pid, "machine starting"}, @assert_receive_timeout
    assert_receive {:runtime_connect_info, ^pid, "connect to node"}, @assert_receive_timeout
    assert_receive {:runtime_connect_info, ^pid, "initialize node"}, @assert_receive_timeout
    assert_receive {:runtime_connect_done, ^pid, {:ok, runtime}}, @assert_receive_timeout

    Runtime.take_ownership(runtime)

    assert [_] = fly_run(fly, ~w(machine list))

    # Verify that we can actually evaluate code on the Fly machine
    Runtime.evaluate_code(runtime, :elixir, ~s/System.fetch_env!("FLY_APP_NAME")/, {:c1, :e1}, [])
    assert_receive {:runtime_evaluation_response, :e1, %{type: :terminal_text, text: text}, _meta}
    assert text =~ fly.app_name

    Runtime.disconnect(runtime)

    # The machine should be automatically destroyed. Blocking in tests
    # is bad, but this test suit is inherently time-consuming and it
    # is opt-in anyway, so it is fine in this case.
    Process.sleep(2000)

    assert [] = fly_run(fly, ~w(machine list))
  end

  test "connecting fails with invalid token" do
    fly = fly!()
    config = config(%{token: "invalid", app_name: fly.app_name})

    pid = Runtime.Fly.new(config) |> Runtime.connect()

    Req.Test.allow(Livebook.FlyAPI, self(), pid)

    assert_receive {:runtime_connect_done, ^pid, {:error, error}}, @assert_receive_timeout
    assert error == "could not create machine, reason: authenticate: token validation error"
  end

  defp config(attrs) do
    defaults = %{
      token: nil,
      app_name: nil,
      region: "fra",
      cpu_kind: "shared",
      cpus: 1,
      memory_gb: 1,
      gpu_kind: nil,
      gpus: nil,
      volume_id: nil,
      docker_tag: "nightly"
    }

    Map.merge(defaults, attrs)
  end

  defp fly_run(fly, args) do
    {output, 0} =
      System.cmd("fly", args ++ ["--app", fly.app_name, "--access-token", fly.token, "--json"])

    JSON.decode!(output)
  end

  defp fly!() do
    token = System.fetch_env!("TEST_FLY_API_TOKEN")
    app_name = System.fetch_env!("TEST_FLY_APP_NAME")
    %{token: token, app_name: app_name}
  end
end

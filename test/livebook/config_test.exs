defmodule Livebook.ConfigTest do
  use ExUnit.Case, async: true

  alias Livebook.Config

  describe "node!/1" do
    test "parses longnames" do
      with_env([TEST_LIVEBOOK_NODE: "test@::1", TEST_LIVEBOOK_DISTRIBUTION: "name"], fn ->
        assert Config.node!("TEST_LIVEBOOK_NODE", "TEST_LIVEBOOK_DISTRIBUTION") ==
                 {:longnames, :"test@::1"}
      end)
    end

    test "parses shortnames" do
      with_env([TEST_LIVEBOOK_NODE: "test", TEST_LIVEBOOK_DISTRIBUTION: "sname"], fn ->
        assert Config.node!("TEST_LIVEBOOK_NODE", "TEST_LIVEBOOK_DISTRIBUTION") ==
                 {:shortnames, :test}
      end)
    end

    test "parses shortnames by default" do
      with_env([TEST_LIVEBOOK_NODE: "test", TEST_LIVEBOOK_DISTRIBUTION: nil], fn ->
        assert Config.node!("TEST_LIVEBOOK_NODE", "TEST_LIVEBOOK_DISTRIBUTION") ==
                 {:shortnames, :test}
      end)
    end

    test "returns nil if node is not set" do
      with_env([TEST_LIVEBOOK_NODE: nil, TEST_LIVEBOOK_DISTRIBUTION: "name"], fn ->
        assert Config.node!("TEST_LIVEBOOK_NODE", "TEST_LIVEBOOK_DISTRIBUTION") == nil
      end)
    end
  end

  defp with_env(env_vars, fun) do
    existing =
      Enum.map(env_vars, fn {env, _value} ->
        {env, env |> to_string() |> System.get_env()}
      end)

    try do
      System.put_env(env_vars)
      fun.()
    after
      System.put_env(existing)
    end
  end
end

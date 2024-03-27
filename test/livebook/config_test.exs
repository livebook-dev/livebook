defmodule Livebook.ConfigTest do
  use ExUnit.Case, async: true

  doctest Livebook.Config
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

  describe "identity_provider!/1" do
    test "parses custom provider" do
      with_env([TEST_IDENTITY_PROVIDER: "custom:Module"], fn ->
        assert Config.identity_provider!("TEST_IDENTITY_PROVIDER") == {:custom, Module, nil}
      end)

      with_env([TEST_IDENTITY_PROVIDER: "custom:LivebookWeb.SessionIdentity:extra"], fn ->
        assert Config.identity_provider!("TEST_IDENTITY_PROVIDER") ==
                 {:custom, LivebookWeb.SessionIdentity, "extra"}
      end)
    end

    test "ZTA provider setting tests" do
      assert {:custom, Livebook.ZTA.GlobalTest, "123"} ==
               Livebook.Config.identity_provider!(
                 "TEST_IDENTITY_custom:Livebook.ZTA.GlobalTest:123"
               )

      assert {:zta, Livebook.ZTA.Cloudflare, "123"} ==
               Livebook.Config.identity_provider!("TEST_IDENTITY_cloudflare:123")

      assert {:session, LivebookWeb.SessionIdentity, :unused} ==
               Livebook.Config.identity_provider!("EMPTY")
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

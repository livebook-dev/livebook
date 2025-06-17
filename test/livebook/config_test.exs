defmodule Livebook.ConfigTest do
  use ExUnit.Case, async: true

  doctest Livebook.Config
  alias Livebook.Config

  describe "rewrite_on!/1" do
    test "parses headers" do
      with_env([TEST_REWRITE_ON: "x-forwarded-for, x-forwarded-proto"], fn ->
        assert Config.rewrite_on!("TEST_REWRITE_ON") == [:x_forwarded_for, :x_forwarded_proto]
      end)
    end
  end

  describe "node!/1" do
    test "parses node" do
      with_env([TEST_LIVEBOOK_NODE: "test@::1"], fn ->
        assert Config.node!("TEST_LIVEBOOK_NODE") == :"test@::1"
      end)
    end
  end

  describe "identity_provider!/1" do
    test "parses custom provider" do
      with_env([TEST_IDENTITY_PROVIDER: "custom:Module"], fn ->
        assert Config.identity_provider!("TEST_IDENTITY_PROVIDER") == {:custom, Module, nil}
      end)

      with_env([TEST_IDENTITY_PROVIDER: "custom:Livebook.ZTA.PassThrough:extra"], fn ->
        assert Config.identity_provider!("TEST_IDENTITY_PROVIDER") ==
                 {:custom, Livebook.ZTA.PassThrough, "extra"}
      end)

      with_env([TEST_IDENTITY_PROVIDER: "cloudflare:123"], fn ->
        assert Config.identity_provider!("TEST_IDENTITY_PROVIDER") ==
                 {:zta, Livebook.ZTA.Cloudflare, "123"}
      end)

      with_env([TEST_IDENTITY_PROVIDER: "basic_auth:user:pass"], fn ->
        assert Config.identity_provider!("TEST_IDENTITY_PROVIDER") ==
                 {:zta, Livebook.ZTA.BasicAuth, "user:pass"}
      end)
    end
  end

  describe "log_metadata!/1" do
    test "parses valida metadata configs" do
      with_env([TEST_LOG_METADATA: "users,request_id"], fn ->
        assert Config.log_metadata!("TEST_LOG_METADATA") == [:users, :request_id]
      end)
    end

    test "returns nil when environment variable is not set" do
      assert Config.log_metadata!("TEST_LOG_METADATA") == nil
    end
  end

  describe "log_format!/1" do
    test "parses valid formats" do
      with_env([TEST_LOG_FORMAT: "text"], fn ->
        assert Config.log_format!("TEST_LOG_FORMAT") == :text
      end)

      with_env([TEST_LOG_FORMAT: "json"], fn ->
        assert Config.log_format!("TEST_LOG_FORMAT") == :json
      end)
    end

    test "returns nil when environment variable is not set" do
      assert Config.log_format!("TEST_LOG_FORMAT") == nil
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

defmodule Livebook.ConfigTest do
  use ExUnit.Case, async: true

  alias Livebook.Config

  describe "node!/1" do
    test "parses a longname from env" do
      with_env([TEST_LIVEBOOK_NAME: "test@::1"], fn ->
        assert Config.node!({"TEST_LIVEBOOK_NAME", "TEST_LIVEBOOK_SNAME"}) ==
                 {:longnames, :"test@::1"}
      end)
    end

    test "parses a shortname from env" do
      with_env([TEST_LIVEBOOK_SNAME: "test"], fn ->
        assert Config.node!({"TEST_LIVEBOOK_NAME", "TEST_LIVEBOOK_SNAME"}) == {:shortnames, :test}
      end)
    end

    test "returns nil if neither longname nor shortname are in env" do
      with_env([TEST_LIVEBOOK_NAME: nil, TEST_LIVEBOOK_SNAME: nil], fn ->
        assert Config.node!({"TEST_LIVEBOOK_NAME", "TEST_LIVEBOOK_SNAME"}) == nil
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

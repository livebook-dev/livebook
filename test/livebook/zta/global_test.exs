defmodule Livebook.ZTA.GlobalTest do
  use ExUnit.Case, async: true

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

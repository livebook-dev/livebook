defmodule Livebook.HubHelpers do
  @moduledoc false

  import ExUnit.Assertions
  import Livebook.Factory
  import Phoenix.LiveViewTest

  @offline_hub %Livebook.Hubs.Team{
    id: "team-org-number-3079",
    teams_key:
      Livebook.Teams.Org.teams_key_prefix() <> "A9TarFeAzmX3sDwSPm5JP5qbLPnNpLpzmjVZUCHXwmI",
    org_public_key:
      Livebook.Hubs.Team.public_key_prefix() <>
        "MIIBCgKCAQEA5v_qciaRGOZd5kgCQbhQDgFCnTnIKI5xzN4m4rVtLXMPH7RTA-K6C-e4wy2gn8zulXgSYX4vXDACSjFAG4PlFhXTPgb-v3rFLwbBrUHdaTMTyxRdK52NyNoDpYklQ7FaEU9vr3Z_-cpAQjdADOV1k45GmFe3bo4gImIfUSDYp1rRiEsYcIBt0Wc0S-vQHKSlmfcCexe254_UkvWjLW7KO790bem-PSWcBI_713oRr2mQoxXeeGKd5dSyFsIr5SZXVRWcRK3soQimCXB0ddBSXZ7d2Md3P9Ylo7TcYdBGHlwVIsrmB-P70KPHPYuAVgS9QsIiiMGXPwYVW77xNRTlcwIDAQAB",
    hub_name: "org-number-3079",
    user_id: 0,
    org_id: 0,
    org_key_id: 0,
    session_token: "",
    offline: %Livebook.Hubs.Team.Offline{
      secrets: []
    }
  }

  def create_team_hub(user, node) do
    hub = build_team_hub(user, node)
    Livebook.Hubs.save_hub(hub)
  end

  def build_team_headers(user, node) do
    hub = build_team_hub(user, node)

    headers = [
      {"x-user", to_string(hub.user_id)},
      {"x-org", to_string(hub.org_id)},
      {"x-org-key", to_string(hub.org_key_id)},
      {"x-session-token", hub.session_token}
    ]

    {hub, headers}
  end

  defp build_team_hub(user, node) do
    teams_org = build(:org)
    teams_key = teams_org.teams_key
    key_hash = Livebook.Teams.Org.key_hash(teams_org)

    org = erpc_call(node, :create_org, [])
    org_key = erpc_call(node, :create_org_key, [[org: org, key_hash: key_hash]])
    org_key_pair = erpc_call(node, :create_org_key_pair, [[org: org]])
    token = erpc_call(node, :associate_user_with_org, [user, org])

    build(:team,
      id: "team-#{org.name}",
      hub_name: org.name,
      user_id: user.id,
      org_id: org.id,
      org_key_id: org_key.id,
      org_public_key: org_key_pair.public_key,
      session_token: token,
      teams_key: teams_key
    )
  end

  def assert_sidebar_hub(view, id, name, emoji \\ "🐈") do
    hub = element(view, hub_element_id(id))
    hub_html = render(hub)

    assert hub_html =~ emoji
    assert hub_html =~ "/hub/#{id}"
    assert hub_html =~ name
  end

  def refute_sidebar_hub(view, id) do
    refute has_element?(view, hub_element_id(id))
  end

  def set_offline_hub() do
    hub = offline_hub()
    ^hub = Livebook.Hubs.save_hub(hub)

    hub
  end

  def offline_hub(), do: @offline_hub

  def put_offline_hub_secret(secret) do
    hub = offline_hub()
    {:ok, pid} = hub_pid(hub)
    {secret_key, sign_secret} = Livebook.Teams.derive_keys(hub.teams_key)
    value = Livebook.Teams.encrypt(secret.value, secret_key, sign_secret)
    secret_created = LivebookProto.SecretCreated.new(name: secret.name, value: value)

    send(pid, {:event, :secret_created, secret_created})
  end

  def remove_offline_hub_secret(secret) do
    hub = offline_hub()
    {:ok, pid} = hub_pid(hub)
    secret_deleted = LivebookProto.SecretDeleted.new(name: secret.name)

    send(pid, {:event, :secret_deleted, secret_deleted})
  end

  defp hub_pid(hub) do
    if pid = GenServer.whereis({:via, Registry, {Livebook.HubsRegistry, hub.id}}) do
      {:ok, pid}
    end
  end

  defp hub_element_id(id), do: "#hubs #hub-#{id}"

  defp erpc_call(node, fun, args) do
    :erpc.call(node, Hub.Integration, fun, args)
  end
end

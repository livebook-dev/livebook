defmodule Livebook.ZTA.GoogleWorkspaceTest do
  use ExUnit.Case, async: true
  use Plug.Test

  alias Livebook.ZTA.GoogleWorkspace

  setup do
    Req.Test.verify_on_exit!()
    :ok
  end

  describe "start_link/1" do
    test "fails to start when the hub doesn't exists", %{test: test} do
      assert_raise RuntimeError, fn ->
        start_supervised!({GoogleWorkspace, name: test, identity_key: "foobarbaz"})
      end
    end
  end

  describe "authenticate/3" do
    setup %{test: test} do
      hub = Livebook.HubHelpers.offline_hub()
      "team-" <> org_name = hub.id

      agent_connected = %LivebookProto.AgentConnected{
        name: "agent-foo",
        agents: [],
        secrets: [],
        file_systems: [],
        app_deployments: [],
        deployment_groups: [],
        public_key: "lb_pk_abc",
        deployment_group_id: "123"
      }

      pid = Livebook.Hubs.TeamClient.get_pid(hub.id)
      send(pid, {:event, :agent_connected, agent_connected})

      options = [
        name: test,
        hub: hub,
        identity_key: org_name,
        req: Req.new(base_url: "http://localhost:12345", plug: {Req.Test, test})
      ]

      {:ok, options: options}
    end

    test "returns the user using the response from external", %{test: test, options: options} do
      start_supervised!({GoogleWorkspace, options})

      expected_token = "yt.token.123"

      expected_identity_data = %{
        id: "1234567890",
        name: "Tuka Peralta",
        email: "tuka@peralta.com",
        payload: %{"token" => expected_token}
      }

      Req.Test.stub(test, fn %{params: %{"access_token" => ^expected_token}} = conn ->
        Req.Test.json(conn, %{
          "sub" => expected_identity_data.id,
          "email" => expected_identity_data.email,
          "name" => expected_identity_data.name
        })
      end)

      conn =
        conn(:get, "/", %{"goog-jwt-access-token" => expected_token})
        |> Plug.Test.init_test_session(%{})

      assert {%{halted: false}, ^expected_identity_data} =
               GoogleWorkspace.authenticate(test, conn, [])
    end

    test "returns the user from session token", %{test: test, options: options} do
      start_supervised!({GoogleWorkspace, options})

      expected_token = "yt.token.123"

      expected_identity_data = %{
        id: "1234567890",
        name: "Tuka Peralta",
        email: "tuka@peralta.com",
        payload: %{"token" => expected_token}
      }

      Req.Test.stub(test, fn %{params: %{"access_token" => ^expected_token}} = conn ->
        Req.Test.json(conn, %{
          "sub" => expected_identity_data.id,
          "email" => expected_identity_data.email,
          "name" => expected_identity_data.name
        })
      end)

      conn =
        conn(:get, "/")
        |> Plug.Test.init_test_session(%{"identity_data" => expected_identity_data})

      assert {%{halted: false}, ^expected_identity_data} =
               GoogleWorkspace.authenticate(test, conn, [])
    end

    test "returns nil with invalid session token", %{test: test, options: options} do
      hub = options[:hub]
      start_supervised!({GoogleWorkspace, options})

      expected_token = "yt.token.123"

      identity_data = %{
        id: "1234567890",
        name: "Tuka Peralta",
        email: "tuka@peralta.com",
        payload: %{"token" => expected_token}
      }

      Req.Test.stub(test, fn %{params: %{"access_token" => ^expected_token}} = conn ->
        conn
        |> put_status(:unauthorized)
        |> Req.Test.json(%{
          error: %{
            errors: [
              %{
                domain: "global",
                reason: "authError",
                message: "Invalid Credentials",
                locationType: "header",
                location: "Authorization"
              }
            ],
            code: 401,
            message: "Invalid Credentials"
          }
        })
      end)

      conn =
        conn(:get, "/")
        |> Plug.Test.init_test_session(%{"identity_data" => identity_data})

      assert {%{halted: true} = conn, nil} = GoogleWorkspace.authenticate(test, conn, [])

      # validate the url to be redirected

      deployment_group_id = Livebook.Hubs.TeamClient.get_deployment_group_id(hub.id)
      assert deployment_group_id
      "team-" <> org_name = hub.id

      zta_url =
        URI.parse(Livebook.Config.teams_url())
        |> URI.append_path("/zta/google_workspace")
        |> URI.to_string()

      redirect_uri = URI.encode_www_form(LivebookWeb.Endpoint.url())

      assert conn.resp_body =~ zta_url
      assert conn.resp_body =~ "org_name=#{org_name}"
      assert conn.resp_body =~ "deployment_group_id=#{deployment_group_id}"
      assert conn.resp_body =~ "redirect_uri=#{redirect_uri}"
    end

    test "returns nil with new session", %{test: test, options: options} do
      hub = options[:hub]
      start_supervised!({GoogleWorkspace, options})

      conn =
        conn(:get, "/")
        |> Plug.Test.init_test_session(%{})

      assert {%{halted: true} = conn, nil} = GoogleWorkspace.authenticate(test, conn, [])

      # validate the url to be redirected

      deployment_group_id = Livebook.Hubs.TeamClient.get_deployment_group_id(hub.id)
      assert deployment_group_id
      "team-" <> org_name = hub.id

      zta_url =
        URI.parse(Livebook.Config.teams_url())
        |> URI.append_path("/zta/google_workspace")
        |> URI.to_string()

      redirect_uri = URI.encode_www_form(LivebookWeb.Endpoint.url())

      assert conn.resp_body =~ zta_url
      assert conn.resp_body =~ "org_name=#{org_name}"
      assert conn.resp_body =~ "deployment_group_id=#{deployment_group_id}"
      assert conn.resp_body =~ "redirect_uri=#{redirect_uri}"
    end
  end
end

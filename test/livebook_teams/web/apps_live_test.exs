defmodule LivebookWeb.Integration.AppsLiveTest do
  use Livebook.TeamsIntegrationCase, async: true

  import Phoenix.LiveViewTest
  import Livebook.AppHelpers

  @moduletag teams_for: :agent
  setup :teams

  @moduletag subscribe_to_hubs_topics: [:connection]
  @moduletag subscribe_to_teams_topics: [
               :clients,
               :agents,
               :deployment_groups,
               :app_deployments,
               :app_server,
               :app_folders
             ]

  @moduletag :tmp_dir

  setup do
    Livebook.Apps.subscribe()
    :ok
  end

  describe "authorized apps" do
    setup :livebook_teams_auth

    test "shows one app if user doesn't have full access",
         %{conn: conn, code: code, node: node, tmp_dir: tmp_dir} = context do
      TeamsRPC.toggle_groups_authorization(node, context.deployment_group)
      oidc_provider = TeamsRPC.create_oidc_provider(node, context.org)
      app_folder = TeamsRPC.create_app_folder(node, org: context.org)

      authorization_group =
        TeamsRPC.create_authorization_group(node,
          group_name: "marketing",
          access_type: :apps,
          app_folders: [app_folder],
          oidc_provider: oidc_provider,
          deployment_group: context.deployment_group
        )

      TeamsRPC.update_user_info_groups(
        node,
        code,
        [
          %{
            "provider_id" => to_string(oidc_provider.id),
            "group_name" => authorization_group.group_name
          }
        ]
      )

      slug = "dev-app-#{Livebook.Utils.random_short_id()}"
      context = change_to_user_session(context)

      deploy_app(
        slug,
        context.team,
        context.org,
        context.deployment_group,
        tmp_dir,
        node,
        app_folder
      )

      change_to_agent_session(context)
      wait_livebook_app_start(slug)

      html =
        conn
        |> get(~p"/apps")
        |> html_response(200)

      refute html =~ "No apps running."
      assert html =~ slug
    end

    test "shows all apps if user have full access",
         %{conn: conn, node: node, code: code, tmp_dir: tmp_dir} = context do
      TeamsRPC.toggle_groups_authorization(node, context.deployment_group)
      oidc_provider = TeamsRPC.create_oidc_provider(node, context.org)

      authorization_group =
        TeamsRPC.create_authorization_group(node,
          group_name: "marketing",
          access_type: :app_server,
          oidc_provider: oidc_provider,
          deployment_group: context.deployment_group
        )

      TeamsRPC.update_user_info_groups(
        node,
        code,
        [
          %{
            "provider_id" => to_string(oidc_provider.id),
            "group_name" => authorization_group.group_name
          }
        ]
      )

      slugs = [
        "mkt-app-#{Livebook.Utils.random_short_id()}",
        "sales-app-#{Livebook.Utils.random_short_id()}",
        "opt-app-#{Livebook.Utils.random_short_id()}"
      ]

      context = change_to_user_session(context)

      for slug <- slugs do
        deploy_app(slug, context.team, context.org, context.deployment_group, tmp_dir, node)
      end

      change_to_agent_session(context)

      for slug <- slugs do
        wait_livebook_app_start(slug)
      end

      html =
        conn
        |> get(~p"/apps")
        |> html_response(200)

      refute html =~ "No apps running."

      for slug <- slugs do
        assert html =~ slug
      end
    end

    test "updates the apps list in real-time",
         %{conn: conn, node: node, code: code, tmp_dir: tmp_dir} = context do
      {:ok, %{groups_auth: true} = deployment_group} =
        TeamsRPC.toggle_groups_authorization(node, context.deployment_group)

      id = to_string(deployment_group.id)
      assert_receive {:deployment_group_updated, %{id: ^id, groups_auth: true}}

      oidc_provider = TeamsRPC.create_oidc_provider(node, context.org)
      app_folder = TeamsRPC.create_app_folder(node, org: context.org)

      authorization_group =
        TeamsRPC.create_authorization_group(node,
          group_name: "marketing",
          access_type: :apps,
          app_folders: [app_folder],
          oidc_provider: oidc_provider,
          deployment_group: deployment_group
        )

      TeamsRPC.update_user_info_groups(
        node,
        code,
        [
          %{
            "provider_id" => to_string(oidc_provider.id),
            "group_name" => authorization_group.group_name
          }
        ]
      )

      slug = "marketing-report-#{Livebook.Utils.random_short_id()}"
      context = change_to_user_session(context)

      deploy_app(
        slug,
        context.team,
        context.org,
        context.deployment_group,
        tmp_dir,
        node
      )

      change_to_agent_session(context)
      wait_livebook_app_start(slug)

      {:ok, view, _} = live(conn, ~p"/apps")
      refute render(view) =~ slug

      {:ok, %{groups_auth: false}} = TeamsRPC.toggle_groups_authorization(node, deployment_group)
      assert_receive {:server_authorization_updated, %{id: ^id, groups_auth: false}}, 3_000

      {:ok, view, _} = live(conn, ~p"/apps")
      assert render(view) =~ slug
    end

    test "shows all apps if disable the authentication in real-time",
         %{conn: conn, node: node, code: code, tmp_dir: tmp_dir} = context do
      {:ok, %{groups_auth: true} = deployment_group} =
        TeamsRPC.toggle_groups_authorization(node, context.deployment_group)

      id = to_string(deployment_group.id)
      assert_receive {:deployment_group_updated, %{id: ^id, groups_auth: true}}

      oidc_provider = TeamsRPC.create_oidc_provider(node, context.org)
      app_folder = TeamsRPC.create_app_folder(node, org: context.org)
      app_folder2 = TeamsRPC.create_app_folder(node, org: context.org)

      authorization_group =
        TeamsRPC.create_authorization_group(node,
          group_name: "marketing",
          access_type: :apps,
          app_folders: [app_folder],
          oidc_provider: oidc_provider,
          deployment_group: deployment_group
        )

      TeamsRPC.update_user_info_groups(
        node,
        code,
        [
          %{
            "provider_id" => to_string(oidc_provider.id),
            "group_name" => authorization_group.group_name
          }
        ]
      )

      slug = "marketing-app-#{Livebook.Utils.random_short_id()}"
      context = change_to_user_session(context)

      deploy_app(
        slug,
        context.team,
        context.org,
        context.deployment_group,
        tmp_dir,
        node,
        app_folder2
      )

      change_to_agent_session(context)
      wait_livebook_app_start(slug)

      {:ok, view, _} = live(conn, ~p"/apps")
      refute render(view) =~ slug

      {:ok, %{teams_auth: false}} = TeamsRPC.toggle_teams_authentication(node, deployment_group)
      assert_receive {:server_authorization_updated, %{id: ^id, teams_auth: false}}, 3_000

      {:ok, view, _} = live(conn, ~p"/apps")
      assert render(view) =~ slug
    end

    test "updates the folder name in real-time",
         %{conn: conn, node: node, code: code, tmp_dir: tmp_dir} = context do
      {:ok, %{groups_auth: true} = deployment_group} =
        TeamsRPC.toggle_groups_authorization(node, context.deployment_group)

      id = to_string(deployment_group.id)
      assert_receive {:deployment_group_updated, %{id: ^id, groups_auth: true}}

      oidc_provider = TeamsRPC.create_oidc_provider(node, context.org)
      app_folder = TeamsRPC.create_app_folder(node, org: context.org)

      authorization_group =
        TeamsRPC.create_authorization_group(node,
          group_name: "marketing",
          access_type: :apps,
          app_folders: [app_folder],
          oidc_provider: oidc_provider,
          deployment_group: deployment_group
        )

      TeamsRPC.update_user_info_groups(
        node,
        code,
        [
          %{
            "provider_id" => to_string(oidc_provider.id),
            "group_name" => authorization_group.group_name
          }
        ]
      )

      slug = Livebook.Utils.random_short_id()
      context = change_to_user_session(context)

      deploy_app(
        slug,
        context.team,
        context.org,
        context.deployment_group,
        tmp_dir,
        node,
        app_folder
      )

      change_to_agent_session(context)
      wait_livebook_app_start(slug)

      {:ok, view, _} = live(conn, ~p"/apps")
      assert render(view) =~ app_folder.name
      assert render(view) =~ slug

      new_name = "NewAppFolderName"
      app_folder_id = to_string(app_folder.id)

      {:ok, _app_folder} = TeamsRPC.update_app_folder(node, app_folder, name: new_name)
      assert_receive {:app_folder_updated, %{id: ^app_folder_id, name: ^new_name}}

      refute render(view) =~ app_folder.name
      assert render(view) =~ new_name
      assert render(view) =~ slug
    end

    test "deletes the folder and move the app to ungrouped apps folder in real-time",
         %{conn: conn, node: node, code: code, tmp_dir: tmp_dir} = context do
      {:ok, %{groups_auth: true} = deployment_group} =
        TeamsRPC.toggle_groups_authorization(node, context.deployment_group)

      id = to_string(deployment_group.id)
      assert_receive {:deployment_group_updated, %{id: ^id, groups_auth: true}}

      oidc_provider = TeamsRPC.create_oidc_provider(node, context.org)
      app_folder = TeamsRPC.create_app_folder(node, org: context.org)

      authorization_group =
        TeamsRPC.create_authorization_group(node,
          group_name: "marketing",
          access_type: :apps,
          app_folders: [app_folder],
          oidc_provider: oidc_provider,
          deployment_group: deployment_group
        )

      TeamsRPC.update_user_info_groups(
        node,
        code,
        [
          %{
            "provider_id" => to_string(oidc_provider.id),
            "group_name" => authorization_group.group_name
          }
        ]
      )

      slug = Livebook.Utils.random_short_id()
      context = change_to_user_session(context)

      deploy_app(
        slug,
        context.team,
        context.org,
        context.deployment_group,
        tmp_dir,
        node,
        app_folder
      )

      change_to_agent_session(context)
      wait_livebook_app_start(slug)

      {:ok, view, _} = live(conn, ~p"/apps")
      assert render(view) =~ app_folder.name
      assert render(view) =~ slug

      id = to_string(deployment_group.id)
      app_folder_id = to_string(app_folder.id)

      TeamsRPC.delete_app_folder(node, app_folder)
      assert_receive {:app_folder_deleted, %{id: ^app_folder_id}}
      assert_receive {:app_deployment_updated, %{slug: ^slug, app_folder_id: nil}}
      assert_receive {:app_updated, %{slug: ^slug, app_spec: %{app_folder_id: ^app_folder_id}}}

      # Once the folder is deleted, all apps are moved to a "Ungrouped apps" folder,
      # which only users with full access will be able to see and access them.
      refute render(view) =~ app_folder.name
      refute render(view) =~ slug

      # To validate this behaivour, updates the authorization group to be full access
      {:ok, %{access_type: :app_server}} =
        TeamsRPC.update_authorization_group(node, authorization_group, %{access_type: :app_server})

      # Since we're updating the authorization group access type, the app deployment must receive the updated version
      assert_receive {:server_authorization_updated, %{id: ^id}}, 3_000
      assert_receive {:app_deployment_updated, %{slug: ^slug, app_folder_id: nil}}

      refute render(view) =~ app_folder.name
      assert render(view) =~ "Ungrouped apps"
      assert render(view) =~ slug
    end
  end
end

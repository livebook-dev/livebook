defmodule LivebookWeb.Integration.SessionLiveTest do
  use Livebook.TeamsIntegrationCase, async: true

  import Phoenix.LiveViewTest
  import Livebook.SessionHelpers

  alias Livebook.{FileSystem, Sessions, Session}

  setup do
    {:ok, session} = Sessions.create_session(notebook: Livebook.Notebook.new())
    Session.subscribe(session.id)

    on_exit(fn ->
      Session.close(session.pid)
    end)

    %{session: session}
  end

  describe "hubs" do
    test "selects the notebook hub", %{conn: conn, user: user, node: node, session: session} do
      hub = create_team_hub(user, node)
      id = hub.id
      personal_id = Livebook.Hubs.Personal.id()

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      assert Session.get_notebook(session.pid).hub_id == personal_id

      view
      |> element(~s/#select-hub-#{id}/)
      |> render_click()

      assert_receive {:operation, {:set_notebook_hub, _, ^id}}
      assert Session.get_notebook(session.pid).hub_id == hub.id
    end

    test "closes all sessions from notebooks that belongs to the org when the org deletes the user",
         %{conn: conn, user: user, node: node, session: session} do
      Livebook.Hubs.Broadcasts.subscribe([:connection, :crud, :secrets])
      Livebook.Teams.Broadcasts.subscribe([:clients])
      Session.subscribe(session.id)

      hub = create_team_hub(user, node)
      id = hub.id

      assert_receive {:hub_connected, ^id}
      assert_receive {:client_connected, ^id}, 10_000

      Session.set_notebook_hub(session.pid, id)

      assert_receive {:operation, {:set_notebook_hub, _, ^id}}
      assert Session.get_notebook(session.pid).hub_id == id

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      assert has_element?(view, ~s/#select-hub-#{id}/)

      # force user to be deleted from org
      erpc_call(node, :delete_user_org, [user.id, hub.org_id])
      reason = "#{hub.hub_name}: you were removed from the org"

      # checks if the hub received the `user_deleted` event and deleted the hub
      assert_receive {:hub_server_error, ^id, ^reason}
      assert_receive {:hub_deleted, ^id}
      refute hub in Livebook.Hubs.get_hubs()

      # all sessions that uses the deleted hub must be closed
      assert_receive :session_closed
    end
  end

  describe "secrets" do
    test "creates a new secret", %{conn: conn, user: user, node: node, session: session} do
      team = create_team_hub(user, node)

      # loads the session page
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      # selects the notebook's hub with team hub id
      view
      |> element(~s/#select-hub-#{team.id}/)
      |> render_click()

      # clicks the button to add a new secret
      view
      |> element("#new-secret-button")
      |> render_click(%{})

      # redirects to secrets action to
      # render the secret modal
      assert_patch(view, ~p"/sessions/#{session.id}/secrets")

      secret =
        build(:secret,
          name: "BIG_IMPORTANT_SECRET",
          value: "123",
          hub_id: team.id
        )

      attrs = %{
        secret: %{
          name: secret.name,
          value: secret.value,
          hub_id: team.id
        }
      }

      # fills and submits the secrets modal form
      # to create a new secret on team hub
      form = element(view, ~s{#secrets-modal form[phx-submit="save"]})

      render_change(form, attrs)
      render_submit(form, attrs)

      # receives the operation event
      assert_receive {:operation, {:sync_hub_secrets, "__server__"}}
      assert secret in Livebook.Hubs.get_secrets(team)

      # checks the secret on the UI
      assert_session_secret(view, session.pid, secret, :hub_secrets)
    end

    test "redirects the user to update or delete a secret",
         %{conn: conn, user: user, node: node, session: session} do
      Livebook.Hubs.Broadcasts.subscribe([:secrets, :connection])
      team = create_team_hub(user, node)
      id = team.id
      assert_receive {:hub_connected, ^id}

      # creates a secret
      secret = insert_secret(hub_id: team.id)
      assert_receive {:secret_created, ^secret}

      # selects the notebook's hub with team hub id
      Session.set_notebook_hub(session.pid, team.id)

      # loads the session page
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      # clicks the button to edit a secret
      view
      |> element("#hub-#{id}-secret-#{secret.name}-edit-button")
      |> render_click()

      # redirects to hub page and loads the modal with
      # the secret name and value filled
      assert_redirect(view, ~p"/hub/#{id}/secrets/edit/#{secret.name}")
    end

    test "toggle a secret from team hub", %{conn: conn, session: session, user: user, node: node} do
      team = create_team_hub(user, node)

      # loads the session page
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      # selects the notebook's hub with team hub id
      Session.set_notebook_hub(session.pid, team.id)

      # creates a new secret
      secret =
        build(:secret,
          name: "POSTGRES_PASSWORD",
          value: "123456789",
          hub_id: team.id
        )

      assert Livebook.Hubs.create_secret(team, secret) == :ok

      # receives the operation event
      assert_receive {:operation, {:sync_hub_secrets, "__server__"}}
      assert secret in Livebook.Hubs.get_secrets(team)

      # checks the secret on the UI
      Session.set_secret(session.pid, secret)
      assert_session_secret(view, session.pid, secret)
    end

    test "adding a missing secret using 'Add secret' button",
         %{conn: conn, user: user, node: node, session: session} do
      team = create_team_hub(user, node)

      secret =
        build(:secret,
          name: "MYSQL_PASS",
          value: "admin",
          hub_id: team.id
        )

      # selects the notebook's hub with team hub id
      Session.set_notebook_hub(session.pid, team.id)

      # executes the code to trigger the `System.EnvError` exception
      # and outputs the 'Add secret' button
      section_id = insert_section(session.pid)
      code = ~s{System.fetch_env!("LB_#{secret.name}")}
      cell_id = insert_text_cell(session.pid, section_id, :code, code)

      Session.queue_cell_evaluation(session.pid, cell_id)
      assert_receive {:operation, {:add_cell_evaluation_response, _, ^cell_id, _, _}}

      # enters the session to check if the button exists
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      expected_url = ~p"/sessions/#{session.id}/secrets?secret_name=#{secret.name}"
      add_secret_button = element(view, "a[href='#{expected_url}']")
      assert has_element?(add_secret_button)

      # clicks the button and fills the form to create a new secret
      # that prefilled the name with the received from exception.
      render_click(add_secret_button)
      form_element = element(view, "#secrets-modal form[phx-submit='save']")
      assert has_element?(form_element)
      attrs = %{value: secret.value, hub_id: team.id}
      render_submit(form_element, %{secret: attrs})

      # receives the operation event
      assert_receive {:operation, {:sync_hub_secrets, "__server__"}}
      assert secret in Livebook.Hubs.get_secrets(team)

      # checks if the secret exists and is inside the session,
      # then executes the code cell again and checks if the
      # secret value is what we expected.
      assert_session_secret(view, session.pid, secret, :hub_secrets)
      Session.queue_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id,
                       %{type: :terminal_text, text: output}, _}}

      assert output == "\e[32m\"#{secret.value}\"\e[0m"
    end

    test "granting access for missing secret using 'Add secret' button",
         %{conn: conn, user: user, node: node, session: session} do
      team = create_team_hub(user, node)

      secret =
        build(:secret,
          name: "PGPASS",
          value: "admin",
          hub_id: team.id
        )

      # selects the notebook's hub with team hub id
      Session.set_notebook_hub(session.pid, team.id)

      # executes the code to trigger the `System.EnvError` exception
      # and outputs the 'Add secret' button
      section_id = insert_section(session.pid)
      code = ~s{System.fetch_env!("LB_#{secret.name}")}
      cell_id = insert_text_cell(session.pid, section_id, :code, code)

      Session.queue_cell_evaluation(session.pid, cell_id)
      assert_receive {:operation, {:add_cell_evaluation_response, _, ^cell_id, _, _}}

      # enters the session to check if the button exists
      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")
      expected_url = ~p"/sessions/#{session.id}/secrets?secret_name=#{secret.name}"
      add_secret_button = element(view, "a[href='#{expected_url}']")
      assert has_element?(add_secret_button)

      # creates the secret
      assert Livebook.Hubs.create_secret(team, secret) == :ok

      # receives the operation event
      assert_receive {:operation, {:sync_hub_secrets, "__server__"}}
      assert secret in Livebook.Hubs.get_secrets(team)

      # remove the secret from session
      Session.unset_secret(session.pid, secret.name)

      # clicks the button and checks if the 'Grant access' banner
      # is being shown, so clicks it's button to set the app secret
      # to the session, allowing the user to fetches the secret.
      render_click(add_secret_button)

      assert render(view) =~
               "in the #{hub_label(team)} workspace. Allow this notebook to access it?"

      view
      |> element("#secrets-modal button", "Grant access")
      |> render_click()

      # checks if the secret exists and is inside the session,
      # then executes the code cell again and checks if the
      # secret value is what we expected.
      assert_session_secret(view, session.pid, secret, :hub_secrets)
      Session.queue_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id,
                       %{type: :terminal_text, text: output}, _}}

      assert output == "\e[32m\"#{secret.value}\"\e[0m"
    end
  end

  describe "files" do
    test "shows only hub's file systems",
         %{conn: conn, user: user, node: node, session: session} do
      Livebook.Hubs.Broadcasts.subscribe([:file_systems])

      personal_id = Livebook.Hubs.Personal.id()
      personal_file_system = build(:fs_s3)
      Livebook.Hubs.Personal.save_file_system(personal_file_system)

      team = create_team_hub(user, node)
      team_id = team.id

      bucket_url = "https://my-own-bucket.s3.amazonaws.com"

      team_file_system =
        build(:fs_s3,
          id: FileSystem.S3.id(team_id, bucket_url),
          bucket_url: bucket_url,
          hub_id: team_id
        )

      Livebook.Hubs.create_file_system(team, team_file_system)
      assert_receive {:file_system_created, %{hub_id: ^team_id} = team_file_system}

      # loads the session page
      {:ok, view, _html} = live(conn, ~p"/sessions/#{session.id}/add-file/storage")

      # change the hub to Personal
      # and checks the file systems from Personal
      Session.set_notebook_hub(session.pid, personal_id)
      assert_receive {:operation, {:set_notebook_hub, _client, ^personal_id}}

      file_entry_select = element(view, "#add-file-entry-select")

      # checks the file systems from Personal
      assert render(file_entry_select) =~ "local"
      assert render(file_entry_select) =~ personal_file_system.id
      refute render(file_entry_select) =~ team_file_system.id

      # change the hub to Team
      # and checks the file systems from Team
      Session.set_notebook_hub(session.pid, team.id)
      assert_receive {:operation, {:set_notebook_hub, _client, ^team_id}}

      assert render(file_entry_select) =~ "local"
      refute render(file_entry_select) =~ personal_file_system.id
      assert render(file_entry_select) =~ team_file_system.id
    end

    test "shows file system from offline hub", %{conn: conn, session: session} do
      Livebook.Hubs.Broadcasts.subscribe([:file_systems])

      hub = offline_hub()
      hub_id = hub.id
      bucket_url = "https://#{hub.id}-file-system.s3.amazonaws.com"

      file_system =
        build(:fs_s3,
          id: FileSystem.S3.id(hub_id, bucket_url),
          bucket_url: bucket_url,
          hub_id: hub_id,
          external_id: "123"
        )

      put_offline_hub_file_system(file_system)
      assert_receive {:file_system_created, ^file_system}

      # loads the session page
      {:ok, view, _html} = live(conn, ~p"/sessions/#{session.id}/add-file/storage")

      # change the hub to Personal
      # and checks the file systems from Offline hub
      Session.set_notebook_hub(session.pid, hub_id)
      assert_receive {:operation, {:set_notebook_hub, _client, ^hub_id}}

      # checks the file systems from Offline hub
      file_entry_select = element(view, "#add-file-entry-select")
      assert render(file_entry_select) =~ "local"
      assert render(file_entry_select) =~ file_system.id

      remove_offline_hub_file_system(file_system)
    end
  end

  describe "offline deployment with docker" do
    @tag :tmp_dir
    test "show deployment group on app deployment",
         %{conn: conn, user: user, node: node, session: session, tmp_dir: tmp_dir} do
      team = create_team_hub(user, node)
      team_id = team.id

      insert_deployment_group(
        name: "DEPLOYMENT_GROUP_SUSIE",
        mode: :online,
        hub_id: team_id
      )

      Session.set_notebook_hub(session.pid, team_id)
      assert_receive {:operation, {:set_notebook_hub, _client, ^team_id}}

      notebook_path = Path.join(tmp_dir, "notebook.livemd")
      file = FileSystem.File.local(notebook_path)
      Session.set_file(session.pid, file)

      slug = Livebook.Utils.random_short_id()
      app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}
      Session.set_app_settings(session.pid, app_settings)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/app-docker")

      assert render(view) =~ "Deployment Group"
      assert has_element?(view, "#select_deployment_group_form")
    end

    @tag :tmp_dir
    test "set deployment group on app deployment",
         %{conn: conn, user: user, node: node, session: session, tmp_dir: tmp_dir} do
      team = create_team_hub(user, node)
      team_id = team.id

      insert_deployment_group(
        name: "DEPLOYMENT_GROUP_SUSIE",
        mode: :online,
        hub_id: team_id
      )

      deployment_group =
        insert_deployment_group(
          name: "DEPLOYMENT_GROUP_TOBIAS",
          mode: :online,
          hub_id: team_id
        )

      Session.set_notebook_hub(session.pid, team_id)
      assert_receive {:operation, {:set_notebook_hub, _client, ^team_id}}

      notebook_path = Path.join(tmp_dir, "notebook.livemd")
      file = FileSystem.File.local(notebook_path)
      Session.set_file(session.pid, file)

      slug = Livebook.Utils.random_short_id()
      app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}
      Session.set_app_settings(session.pid, app_settings)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/app-docker")

      assert render(view) =~ "Deployment Group"
      assert has_element?(view, "#select_deployment_group_form")

      id = deployment_group.id

      view
      |> form("#select_deployment_group_form", %{deployment_group: %{id: id}})
      |> render_change()

      assert_receive {:operation, {:set_notebook_deployment_group, _client, ^id}}
    end

    @tag :tmp_dir
    test "show no deployments groups available",
         %{conn: conn, user: user, node: node, session: session, tmp_dir: tmp_dir} do
      team = create_team_hub(user, node)
      team_id = team.id

      Session.set_notebook_hub(session.pid, team_id)
      assert_receive {:operation, {:set_notebook_hub, _client, ^team_id}}

      notebook_path = Path.join(tmp_dir, "notebook.livemd")
      file = FileSystem.File.local(notebook_path)
      Session.set_file(session.pid, file)

      slug = Livebook.Utils.random_short_id()
      app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}
      Session.set_app_settings(session.pid, app_settings)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/app-docker")

      assert render(view) =~ "Deployment Group"
      assert render(view) =~ "None configured"
      refute has_element?(view, "#select_deployment_group_form")
    end
  end

  describe "online deployment" do
    test "shows a message when non-teams hub is selected",
         %{conn: conn, user: user, node: node, session: session} do
      create_team_hub(user, node)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element("a", "Deploy with Livebook Teams")
      |> render_click()

      assert render(view) =~
               "In order to deploy your app using Livebook Teams, you need to select"
    end

    test "deployment flow with no deployment groups in the hub",
         %{conn: conn, user: user, node: node, session: session} do
      team = create_team_hub(user, node)
      Session.set_notebook_hub(session.pid, team.id)

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element("a", "Deploy with Livebook Teams")
      |> render_click()

      # Step: configuring valid app settings

      assert render(view) =~ "You must configure your app before deploying it."

      slug = Livebook.Utils.random_short_id()

      view
      |> element(~s/#app-settings-modal form/)
      |> render_submit(%{"app_settings" => %{"slug" => slug}})

      # From this point forward we are in a child LV
      view = find_live_child(view, "app-teams")
      assert render(view) =~ "App deployment with Livebook Teams"

      # Step: deployment group creation

      assert render(view) =~ "Step: add deployment group"
      assert render(view) =~ "You must create a deployment group before deploying the app."

      view
      |> element(~s/#add-deployment-group-form/)
      |> render_submit(%{"deployment_group" => %{"name" => "test"}})

      # Step: agent instance setup

      assert render(view) =~ "Step: add app server"
      assert render(view) =~ "You must set up an app server for the app to run on."

      assert render(view) =~ "Awaiting an app server to be set up."

      [deployment_group] = Livebook.Hubs.TeamClient.get_deployment_groups(team.id)
      simulate_agent_join(team, deployment_group)

      assert render(view) =~ "An app server is running"

      # Step: deploy

      view
      |> element("button", "Deploy")
      |> render_click()

      assert render(view) =~
               "App deployment created successfully"

      assert render(view) =~ "#{Livebook.Config.teams_url()}/orgs/#{team.org_id}"
    end

    test "deployment flow with existing deployment groups in the hub",
         %{conn: conn, user: user, node: node, session: session} do
      Livebook.Teams.Broadcasts.subscribe([:deployment_groups])
      team = create_team_hub(user, node)
      Session.set_notebook_hub(session.pid, team.id)

      id = insert_deployment_group(mode: :online, hub_id: team.id).id
      assert_receive {:deployment_group_created, %{id: ^id} = deployment_group}

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}")

      view
      |> element("a", "Deploy with Livebook Teams")
      |> render_click()

      # Step: configuring valid app settings

      assert render(view) =~ "You must configure your app before deploying it."

      slug = Livebook.Utils.random_short_id()

      view
      |> element(~s/#app-settings-modal form/)
      |> render_submit(%{"app_settings" => %{"slug" => slug}})

      # From this point forward we are in a child LV
      view = find_live_child(view, "app-teams")
      assert render(view) =~ "App deployment with Livebook Teams"

      # Step: selecting deployment group

      view
      |> element(~s/[phx-click="select_deployment_group"][phx-value-id="#{deployment_group.id}"]/)
      |> render_click()

      assert_receive {:operation, {:set_notebook_deployment_group, _, ^id}}
      assert render(view) =~ "The selected deployment group has no app servers."

      view
      |> element(~s/button/, "Add app server")
      |> render_click()

      # Step: agent instance setup

      assert render(view) =~ "Step: add app server"
      assert render(view) =~ "Awaiting an app server to be set up."

      [deployment_group] = Livebook.Hubs.TeamClient.get_deployment_groups(team.id)
      simulate_agent_join(team, deployment_group)

      assert render(view) =~ "An app server is running"

      # Step: deploy

      view
      |> element("button", "Deploy")
      |> render_click()

      assert render(view) =~
               "App deployment created successfully"
    end

    test "shows an error when the deployment size is higher than the maximum size of 20MB",
         %{conn: conn, user: user, node: node, session: session} do
      Livebook.Teams.Broadcasts.subscribe([:deployment_groups])
      team = create_team_hub(user, node)
      Session.set_notebook_hub(session.pid, team.id)

      slug = Livebook.Utils.random_short_id()
      app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}
      Session.set_app_settings(session.pid, app_settings)

      id = insert_deployment_group(mode: :online, hub_id: team.id).id
      assert_receive {:deployment_group_created, %{id: ^id}}

      Session.set_notebook_deployment_group(session.pid, id)
      assert_receive {:operation, {:set_notebook_deployment_group, _, ^id}}

      %{files_dir: files_dir} = session
      image_file = FileSystem.File.resolve(files_dir, "image.jpg")
      :ok = FileSystem.File.write(image_file, :crypto.strong_rand_bytes(20 * 1024 * 1024))
      Session.add_file_entries(session.pid, [%{type: :attachment, name: "image.jpg"}])

      {:ok, view, _} = live(conn, ~p"/sessions/#{session.id}/app-teams")

      # From this point forward we are in a child LV
      view = find_live_child(view, "app-teams")
      assert render(view) =~ "App deployment with Livebook Teams"

      view
      |> element("button", "Deploy")
      |> render_click()

      assert render(view) =~
               "Failed to pack files: the notebook and its attachments have exceeded the maximum size of 20MB"
    end
  end
end

defmodule Livebook.Hubs.DockerfileTest do
  use ExUnit.Case, async: true

  import Livebook.TestHelpers

  alias Livebook.Hubs.Dockerfile
  alias Livebook.Hubs
  alias Livebook.Secrets.Secret

  @version if Livebook.Config.app_version() =~ "-dev",
             do: "nightly",
             else: Livebook.Config.app_version()

  describe "airgapped_dockerfile/7" do
    test "deploying a single notebook in personal hub" do
      config = dockerfile_config()
      hub = personal_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = Dockerfile.airgapped_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile == """
             FROM ghcr.io/livebook-dev/livebook:#{@version}

             # Apps configuration
             ENV LIVEBOOK_APPS_PATH="/apps"
             ENV LIVEBOOK_APPS_PATH_WARMUP="manual"

             # Notebook
             COPY notebook.livemd /apps/

             # Cache apps setup at build time
             RUN /app/bin/warmup_apps
             """

      # With secrets

      secret = %Secret{name: "TEST", value: "test", hub_id: hub.id}
      unused_secret = %Secret{name: "TEST2", value: "test", hub_id: hub.id}
      session_secret = %Secret{name: "SESSION", value: "test", hub_id: nil}

      hub_secrets = [secret, unused_secret]
      secrets = %{"TEST" => secret, "SESSION" => session_secret}

      dockerfile =
        Dockerfile.airgapped_dockerfile(config, hub, hub_secrets, [], file, [], secrets)

      assert dockerfile =~
               """
               # Personal Hub secrets
               ENV LB_TEST="test"
               """

      refute dockerfile =~ "ENV LB_SESSION"
    end

    test "deploying a directory in personal hub" do
      config = dockerfile_config(%{deploy_all: true})
      hub = personal_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = Dockerfile.airgapped_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile =~ """
             # Notebooks and files
             COPY . /apps
             """

      # With secrets

      secret = %Secret{name: "TEST", value: "test", hub_id: hub.id}
      unused_secret = %Secret{name: "TEST2", value: "test", hub_id: hub.id}
      session_secret = %Secret{name: "SESSION", value: "test", hub_id: nil}

      hub_secrets = [secret, unused_secret]
      secrets = %{"TEST" => secret, "SESSION" => session_secret}

      dockerfile =
        Dockerfile.airgapped_dockerfile(config, hub, hub_secrets, [], file, [], secrets)

      assert dockerfile =~
               """
               # Personal Hub secrets
               ENV LB_TEST="test"
               ENV LB_TEST2="test"
               """

      refute dockerfile =~ "ENV LB_SESSION"
    end

    test "deploying a single notebook in teams hub" do
      config = dockerfile_config()
      hub = team_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = Dockerfile.airgapped_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile == """
             FROM ghcr.io/livebook-dev/livebook:#{@version}

             ARG TEAMS_KEY="lb_tk_fn0pL3YLWzPoPFWuHeV3kd0o7_SFuIOoU4C_k6OWDYg"

             # Teams Hub configuration for airgapped deployment
             ENV LIVEBOOK_TEAMS_KEY=${TEAMS_KEY}
             ENV LIVEBOOK_TEAMS_AUTH="offline:org-name-387:lb_opk_fpxnp3r5djwxnmirx3tu276hialoivf3"

             # Apps configuration
             ENV LIVEBOOK_APPS_PATH="/apps"
             ENV LIVEBOOK_APPS_PATH_WARMUP="manual"

             # Notebook
             COPY notebook.livemd /apps/

             # Cache apps setup at build time
             RUN /app/bin/warmup_apps
             """

      # With secrets

      secret = %Secret{name: "TEST", value: "test", hub_id: hub.id}
      session_secret = %Secret{name: "SESSION", value: "test", hub_id: nil}

      hub_secrets = [secret]
      secrets = %{"TEST" => secret, "SESSION" => session_secret}

      dockerfile =
        Dockerfile.airgapped_dockerfile(config, hub, hub_secrets, [], file, [], secrets)

      assert dockerfile =~ "ENV LIVEBOOK_TEAMS_SECRETS"
      refute dockerfile =~ "ENV TEST"
      refute dockerfile =~ "ENV LB_SESSION"

      # With file systems

      file_system = Livebook.Factory.build(:fs_s3)
      file_systems = [file_system]

      dockerfile = Dockerfile.airgapped_dockerfile(config, hub, [], file_systems, file, [], %{})

      assert dockerfile =~ "ENV LIVEBOOK_TEAMS_FS"
    end

    test "deploying a directory in teams hub" do
      config = dockerfile_config(%{deploy_all: true})
      hub = team_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = Dockerfile.airgapped_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile =~ """
             # Notebooks and files
             COPY . /apps
             """
    end

    test "deploying with different base image" do
      config = dockerfile_config(%{docker_tag: "#{@version}-cuda12"})
      hub = personal_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = Dockerfile.airgapped_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile =~ """
             FROM ghcr.io/livebook-dev/livebook:#{@version}-cuda12
             """
    end

    test "deploying with file entries" do
      config = dockerfile_config()
      hub = personal_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      file_entries = [
        %{type: :attachment, name: "image.jpeg"},
        %{type: :attachment, name: "data.csv"}
      ]

      dockerfile = Dockerfile.airgapped_dockerfile(config, hub, [], [], file, file_entries, %{})

      assert dockerfile =~
               """
               # Files
               COPY files/data.csv files/image.jpeg /apps/files/
               """
    end

    test "deploying with auto cluster setup" do
      config = dockerfile_config(%{clustering: :auto})
      hub = personal_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = Dockerfile.airgapped_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile =~ ~s/ENV LIVEBOOK_CLUSTER="auto"/
    end

    test "deploying with dns cluster setup" do
      config = dockerfile_config(%{clustering: :dns})
      hub = personal_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = Dockerfile.airgapped_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile =~ ~s/ENV LIVEBOOK_NODE="livebook_server@MACHINE_IP"/
      assert dockerfile =~ ~s/ENV LIVEBOOK_CLUSTER="dns:QUERY"/
    end

    test "deploying with deployment group environment variables" do
      config = %{
        dockerfile_config()
        | environment_variables: [
            {"LIVEBOOK_IDENTITY_PROVIDER", "cloudflare:foobar"},
            {"LIVEBOOK_TEAMS_URL", "http://localhost:8000"},
            {"MY_JSON", ~S|{"foo": "bar"}|}
          ]
      }

      hub = team_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = Dockerfile.airgapped_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile =~ """
             # Deployment group environment variables
             ENV LIVEBOOK_IDENTITY_PROVIDER="cloudflare:foobar"
             ENV LIVEBOOK_TEAMS_URL="http://localhost:8000"
             ENV MY_JSON="{\\"foo\\": \\"bar\\"}"\
             """
    end
  end

  describe "online_docker_info/3" do
    test "includes agent authentication env vars" do
      config = dockerfile_config()
      hub = team_hub()
      agent_key = Livebook.Factory.build(:agent_key)

      %{env: env} = Dockerfile.online_docker_info(config, hub, agent_key)

      assert env == [
               {"LIVEBOOK_AGENT_NAME", "default"},
               {"LIVEBOOK_TEAMS_KEY", "lb_tk_fn0pL3YLWzPoPFWuHeV3kd0o7_SFuIOoU4C_k6OWDYg"},
               {"LIVEBOOK_TEAMS_AUTH",
                "online:org-name-387:1:1:lb_ak_zj9tWM1rEVeweYR7DbH_2VK5_aKtWfptcL07dBncqg"}
             ]
    end

    test "deploying with different base image" do
      config = dockerfile_config(%{docker_tag: "#{@version}-cuda12"})
      hub = team_hub()
      agent_key = Livebook.Factory.build(:agent_key)

      %{image: image, env: _env} = Dockerfile.online_docker_info(config, hub, agent_key)

      assert image == "ghcr.io/livebook-dev/livebook:#{@version}-cuda12"
    end

    test "deploying with auto cluster setup" do
      config = dockerfile_config(%{clustering: :auto})
      hub = team_hub()
      agent_key = Livebook.Factory.build(:agent_key)

      %{env: env} = Dockerfile.online_docker_info(config, hub, agent_key)

      assert {"LIVEBOOK_CLUSTER", "auto"} in env
    end

    test "deploying with dns cluster setup" do
      config = dockerfile_config(%{clustering: :dns})
      hub = team_hub()
      agent_key = Livebook.Factory.build(:agent_key)

      %{env: env} = Dockerfile.online_docker_info(config, hub, agent_key)

      assert {"LIVEBOOK_NODE", "livebook_server@MACHINE_IP"} in env
      assert {"LIVEBOOK_CLUSTER", "dns:QUERY"} in env
    end

    test "deploying with deployment group environment variables" do
      config = %{
        dockerfile_config()
        | environment_variables: %{
            "LIVEBOOK_IDENTITY_PROVIDER" => "cloudflare:foobar",
            "LIVEBOOK_TEAMS_URL" => "http://localhost:8000"
          }
      }

      hub = team_hub()
      agent_key = Livebook.Factory.build(:agent_key)

      %{env: env} = Dockerfile.online_docker_info(config, hub, agent_key)

      assert {"LIVEBOOK_IDENTITY_PROVIDER", "cloudflare:foobar"} in env
      assert {"LIVEBOOK_TEAMS_URL", "http://localhost:8000"} in env
    end
  end

  describe "warnings/6" do
    test "warns when session secrets are used" do
      config = dockerfile_config(%{clustering: :auto})
      hub = personal_hub()
      app_settings = Livebook.Notebook.AppSettings.new()

      session_secret = %Secret{name: "SESSION", value: "test", hub_id: nil}
      secrets = %{"SESSION" => session_secret}

      assert [warning] =
               Dockerfile.airgapped_warnings(config, hub, [], [], app_settings, [], secrets)

      assert warning =~ "The notebook uses session secrets"
    end

    test "warns when hub secrets are used from personal hub" do
      config = dockerfile_config(%{clustering: :auto})
      hub = personal_hub()
      app_settings = Livebook.Notebook.AppSettings.new()

      secret = %Secret{name: "TEST", value: "test", hub_id: hub.id}

      hub_secrets = [secret]
      secrets = %{"TEST" => secret}

      assert [warning] =
               Dockerfile.airgapped_warnings(
                 config,
                 hub,
                 hub_secrets,
                 [],
                 app_settings,
                 [],
                 secrets
               )

      assert warning =~ "secrets are included in the Dockerfile"
    end

    test "warns when there is a reference to external file system from personal hub" do
      config = dockerfile_config(%{clustering: :auto})
      hub = personal_hub()
      app_settings = Livebook.Notebook.AppSettings.new()

      file_system = Livebook.Factory.build(:fs_s3)
      file_systems = [file_system]

      file_entries = [
        %{type: :file, file: Livebook.FileSystem.File.new(file_system, "/data.csv")}
      ]

      assert [warning] =
               Dockerfile.airgapped_warnings(
                 config,
                 hub,
                 [],
                 file_systems,
                 app_settings,
                 file_entries,
                 %{}
               )

      assert warning =~
               "The S3 file storage, configured in your personal workspace, will not be available in the Docker image"
    end

    test "warns when deploying a directory in personal hub and it has any file systems" do
      config = dockerfile_config(%{clustering: :auto, deploy_all: true})
      hub = personal_hub()
      app_settings = Livebook.Notebook.AppSettings.new()

      file_system = Livebook.Factory.build(:fs_s3)
      file_systems = [file_system]

      assert [warning] =
               Dockerfile.airgapped_warnings(config, hub, [], file_systems, app_settings, [], %{})

      assert warning =~
               "The S3 file storage, configured in your personal workspace, will not be available in the Docker image"
    end

    test "warns when the app has no password in personal hub" do
      config = dockerfile_config(%{clustering: :auto})
      hub = personal_hub()
      app_settings = %{Livebook.Notebook.AppSettings.new() | access_type: :public}

      assert [warning] = Dockerfile.airgapped_warnings(config, hub, [], [], app_settings, [], %{})
      assert warning =~ "This app has no password configuration"
    end

    test "warns when no clustering is configured" do
      config = dockerfile_config()
      hub = team_hub()
      app_settings = %{Livebook.Notebook.AppSettings.new() | access_type: :private}

      assert [warning] = Dockerfile.airgapped_warnings(config, hub, [], [], app_settings, [], %{})
      assert warning =~ "Clustering has not been configured for this deployment"

      config = %{config | clustering: :auto}

      assert [] = Dockerfile.airgapped_warnings(config, hub, [], [], app_settings, [], %{})
    end
  end

  defp dockerfile_config(attrs \\ %{}) do
    Dockerfile.config_new()
    |> Dockerfile.config_changeset(attrs)
    |> Ecto.Changeset.apply_changes()
  end

  defp personal_hub() do
    Hubs.fetch_hub!(Hubs.Personal.id())
  end

  defp team_hub() do
    Livebook.Factory.build(:team,
      id: "team-org-name-387",
      hub_name: "org-name-387",
      teams_key: "lb_tk_fn0pL3YLWzPoPFWuHeV3kd0o7_SFuIOoU4C_k6OWDYg",
      org_public_key: "lb_opk_fpxnp3r5djwxnmirx3tu276hialoivf3"
    )
  end
end

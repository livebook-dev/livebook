defmodule Livebook.Hubs.DockerfileTest do
  use ExUnit.Case, async: true

  import Livebook.TestHelpers

  alias Livebook.Hubs.Dockerfile
  alias Livebook.Hubs
  alias Livebook.Secrets.Secret

  describe "build_dockerfile/7" do
    test "deploying a single notebook in personal hub" do
      config = dockerfile_config()
      hub = personal_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = Dockerfile.build_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile == """
             FROM ghcr.io/livebook-dev/livebook:latest

             # Apps configuration
             ENV LIVEBOOK_APPS_PATH "/apps"
             ENV LIVEBOOK_APPS_PATH_WARMUP "manual"
             ENV LIVEBOOK_APPS_PATH_HUB_ID "personal-hub"

             # Notebook
             COPY notebook.livemd /apps/

             # Cache apps setup at build time
             RUN /app/bin/warmup_apps.sh
             """

      # With secrets

      secret = %Secret{name: "TEST", value: "test", hub_id: hub.id}
      unused_secret = %Secret{name: "TEST2", value: "test", hub_id: hub.id}
      session_secret = %Secret{name: "SESSION", value: "test", hub_id: nil}

      hub_secrets = [secret, unused_secret]
      secrets = %{"TEST" => secret, "SESSION" => session_secret}

      dockerfile = Dockerfile.build_dockerfile(config, hub, hub_secrets, [], file, [], secrets)

      assert dockerfile =~
               """
               # Personal Hub secrets
               ENV LB_TEST "test"
               """

      refute dockerfile =~ "ENV LB_SESSION"
    end

    test "deploying a directory in personal hub" do
      config = dockerfile_config(%{deploy_all: true})
      hub = personal_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = Dockerfile.build_dockerfile(config, hub, [], [], file, [], %{})

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

      dockerfile = Dockerfile.build_dockerfile(config, hub, hub_secrets, [], file, [], secrets)

      assert dockerfile =~
               """
               # Personal Hub secrets
               ENV LB_TEST "test"
               ENV LB_TEST2 "test"
               """

      refute dockerfile =~ "ENV LB_SESSION"
    end

    test "deploying a single notebook in teams hub" do
      config = dockerfile_config()
      hub = team_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = Dockerfile.build_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile == """
             FROM ghcr.io/livebook-dev/livebook:latest

             ARG TEAMS_KEY="lb_tk_fn0pL3YLWzPoPFWuHeV3kd0o7_SFuIOoU4C_k6OWDYg"

             # Teams Hub configuration for airgapped deployment
             ENV LIVEBOOK_TEAMS_KEY ${TEAMS_KEY}
             ENV LIVEBOOK_TEAMS_NAME "org-name-387"
             ENV LIVEBOOK_TEAMS_OFFLINE_KEY "lb_opk_fpxnp3r5djwxnmirx3tu276hialoivf3"

             # Apps configuration
             ENV LIVEBOOK_APPS_PATH "/apps"
             ENV LIVEBOOK_APPS_PATH_WARMUP "manual"
             ENV LIVEBOOK_APPS_PATH_HUB_ID "team-org-name-387"

             # Notebook
             COPY notebook.livemd /apps/

             # Cache apps setup at build time
             RUN /app/bin/warmup_apps.sh
             """

      # With secrets

      secret = %Secret{name: "TEST", value: "test", hub_id: hub.id}
      session_secret = %Secret{name: "SESSION", value: "test", hub_id: nil}

      hub_secrets = [secret]
      secrets = %{"TEST" => secret, "SESSION" => session_secret}

      dockerfile = Dockerfile.build_dockerfile(config, hub, hub_secrets, [], file, [], secrets)

      assert dockerfile =~ "ENV LIVEBOOK_TEAMS_SECRETS"
      refute dockerfile =~ "ENV TEST"
      refute dockerfile =~ "ENV LB_SESSION"

      # With file systems

      file_system = Livebook.Factory.build(:fs_s3)
      file_systems = [file_system]

      dockerfile = Dockerfile.build_dockerfile(config, hub, [], file_systems, file, [], %{})

      assert dockerfile =~ "ENV LIVEBOOK_TEAMS_FS"
    end

    test "deploying with ZTA in teams hub" do
      config = dockerfile_config(%{zta_provider: :cloudflare, zta_key: "cloudflare_key"})
      hub = team_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = Dockerfile.build_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile =~ ~S/ENV LIVEBOOK_IDENTITY_PROVIDER "cloudflare:cloudflare_key"/
    end

    test "deploying a directory in teams hub" do
      config = dockerfile_config(%{deploy_all: true})
      hub = team_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = Dockerfile.build_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile =~ """
             # Notebooks and files
             COPY . /apps
             """
    end

    test "deploying with different base image" do
      config = dockerfile_config(%{docker_tag: "latest-cuda11.8"})
      hub = personal_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = Dockerfile.build_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile =~ """
             FROM ghcr.io/livebook-dev/livebook:latest-cuda11.8

             ENV XLA_TARGET "cuda118"
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

      dockerfile = Dockerfile.build_dockerfile(config, hub, [], [], file, file_entries, %{})

      assert dockerfile =~
               """
               # Files
               COPY files/data.csv files/image.jpeg /apps/files/
               """
    end
  end

  describe "warnings/6" do
    test "warns when session secrets are used" do
      config = dockerfile_config()
      hub = personal_hub()
      app_settings = Livebook.Notebook.AppSettings.new()

      session_secret = %Secret{name: "SESSION", value: "test", hub_id: nil}
      secrets = %{"SESSION" => session_secret}

      assert [warning] = Dockerfile.warnings(config, hub, [], app_settings, [], secrets)
      assert warning =~ "The notebook uses session secrets"
    end

    test "warns when hub secrets are used from personal hub" do
      config = dockerfile_config()
      hub = personal_hub()
      app_settings = Livebook.Notebook.AppSettings.new()

      secret = %Secret{name: "TEST", value: "test", hub_id: hub.id}

      hub_secrets = [secret]
      secrets = %{"TEST" => secret}

      assert [warning] = Dockerfile.warnings(config, hub, hub_secrets, app_settings, [], secrets)
      assert warning =~ "secrets are included in the Dockerfile"
    end

    test "warns when there is a reference to external file system from personal hub" do
      config = dockerfile_config()
      hub = personal_hub()
      app_settings = Livebook.Notebook.AppSettings.new()

      file_system = Livebook.Factory.build(:fs_s3)

      file_entries = [
        %{type: :file, file: Livebook.FileSystem.File.new(file_system, "/data.csv")}
      ]

      assert [warning] = Dockerfile.warnings(config, hub, [], app_settings, file_entries, %{})

      assert warning =~
               "The S3 file storage, defined in your personal hub, will not be available in the Docker image"
    end

    test "warns when the app has no password in personal hub" do
      config = dockerfile_config()
      hub = personal_hub()
      app_settings = %{Livebook.Notebook.AppSettings.new() | access_type: :public}

      assert [warning] = Dockerfile.warnings(config, hub, [], app_settings, [], %{})
      assert warning =~ "This app has no password configuration"
    end

    test "warns when the app has no password and no ZTA in teams hub" do
      config = dockerfile_config()
      hub = team_hub()
      app_settings = %{Livebook.Notebook.AppSettings.new() | access_type: :public}

      assert [warning] = Dockerfile.warnings(config, hub, [], app_settings, [], %{})
      assert warning =~ "This app has no password configuration"

      config = %{config | zta_provider: :cloudflare, zta_key: "key"}

      assert [] = Dockerfile.warnings(config, hub, [], app_settings, [], %{})
    end
  end

  defp dockerfile_config(attrs \\ %{}) do
    attrs
    |> Dockerfile.config_changeset()
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

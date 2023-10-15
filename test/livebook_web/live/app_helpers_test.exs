defmodule LivebookWeb.AppHelpersTest do
  use ExUnit.Case, async: true

  import Livebook.TestHelpers

  alias LivebookWeb.AppHelpers
  alias Livebook.Hubs
  alias Livebook.Secrets.Secret

  describe "build_dockerfile/7" do
    test "deploying a single notebook in personal hub" do
      config =
        %{}
        |> AppHelpers.docker_config_changeset()
        |> Ecto.Changeset.apply_changes()

      hub = Hubs.fetch_hub!(Hubs.Personal.id())
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = AppHelpers.build_dockerfile(config, hub, [], [], file, [], %{})

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

      # Different base image

      config =
        %{docker_tag: "latest-cuda11.8"}
        |> AppHelpers.docker_config_changeset()
        |> Ecto.Changeset.apply_changes()

      dockerfile = AppHelpers.build_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile =~ """
             FROM ghcr.io/livebook-dev/livebook:latest-cuda11.8

             ENV XLA_TARGET "cuda118"
             """

      # With files

      file_entries = [
        %{type: :attachment, name: "image.jpeg"},
        %{type: :attachment, name: "data.csv"}
      ]

      dockerfile = AppHelpers.build_dockerfile(config, hub, [], [], file, file_entries, %{})

      assert dockerfile =~
               """
               # Files
               COPY files/data.csv files/image.jpeg /apps/files/
               """

      # With secrets

      secret = %Secret{name: "TEST", value: "test", hub_id: hub.id}
      session_secret = %Secret{name: "SESSION", value: "test", hub_id: nil}

      hub_secrets = [secret]
      secrets = %{"TEST" => secret, "SESSION" => session_secret}

      dockerfile = AppHelpers.build_dockerfile(config, hub, hub_secrets, [], file, [], secrets)

      assert dockerfile =~
               """
               # Personal Hub secrets
               ENV LB_TEST "test"
               """

      refute dockerfile =~ "ENV LB_SESSION"
    end

    test "deploying a directory in personal hub" do
      config =
        %{deploy_all: true}
        |> AppHelpers.docker_config_changeset()
        |> Ecto.Changeset.apply_changes()

      hub = Hubs.fetch_hub!(Hubs.Personal.id())
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = AppHelpers.build_dockerfile(config, hub, [], [], file, [], %{})

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

      dockerfile = AppHelpers.build_dockerfile(config, hub, hub_secrets, [], file, [], secrets)

      assert dockerfile =~
               """
               # Personal Hub secrets
               ENV LB_TEST "test"
               ENV LB_TEST2 "test"
               """

      refute dockerfile =~ "ENV LB_SESSION"
    end

    test "deploying a single notebook in teams hub" do
      config =
        %{}
        |> AppHelpers.docker_config_changeset()
        |> Ecto.Changeset.apply_changes()

      hub = team_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = AppHelpers.build_dockerfile(config, hub, [], [], file, [], %{})

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

      # Different base image

      config =
        %{docker_tag: "latest-cuda11.8"}
        |> AppHelpers.docker_config_changeset()
        |> Ecto.Changeset.apply_changes()

      dockerfile = AppHelpers.build_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile =~ """
             FROM ghcr.io/livebook-dev/livebook:latest-cuda11.8

             ENV XLA_TARGET "cuda118"
             """

      # With files

      file_entries = [
        %{type: :attachment, name: "image.jpeg"},
        %{type: :attachment, name: "data.csv"}
      ]

      dockerfile = AppHelpers.build_dockerfile(config, hub, [], [], file, file_entries, %{})

      assert dockerfile =~
               """
               # Files
               COPY files/data.csv files/image.jpeg /apps/files/
               """

      # With secrets

      secret = %Secret{name: "TEST", value: "test", hub_id: hub.id}
      session_secret = %Secret{name: "SESSION", value: "test", hub_id: nil}

      hub_secrets = [secret]
      secrets = %{"TEST" => secret, "SESSION" => session_secret}

      dockerfile = AppHelpers.build_dockerfile(config, hub, hub_secrets, [], file, [], secrets)

      assert dockerfile =~ "ENV LIVEBOOK_TEAMS_SECRETS"
      refute dockerfile =~ "ENV TEST"
      refute dockerfile =~ "ENV LB_SESSION"

      # With file systems

      file_system = Livebook.Factory.build(:fs_s3)
      file_systems = [file_system]

      dockerfile = AppHelpers.build_dockerfile(config, hub, [], file_systems, file, [], %{})

      assert dockerfile =~ "ENV LIVEBOOK_TEAMS_FS"

      # With ZTA

      config =
        %{zta_provider: :cloudflare, zta_key: "cloudflare_key"}
        |> AppHelpers.docker_config_changeset()
        |> Ecto.Changeset.apply_changes()

      dockerfile = AppHelpers.build_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile =~ ~S/ENV LIVEBOOK_IDENTITY_PROVIDER "cloudflare:cloudflare_key"/
    end

    test "deploying a directory in teams hub" do
      config =
        %{deploy_all: true}
        |> AppHelpers.docker_config_changeset()
        |> Ecto.Changeset.apply_changes()

      hub = team_hub()
      file = Livebook.FileSystem.File.local(p("/notebook.livemd"))

      dockerfile = AppHelpers.build_dockerfile(config, hub, [], [], file, [], %{})

      assert dockerfile =~ """
             # Notebooks and files
             COPY . /apps
             """
    end
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

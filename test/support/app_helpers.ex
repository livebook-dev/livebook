defmodule Livebook.AppHelpers do
  import ExUnit.{Assertions, Callbacks}

  alias Livebook.TeamsRPC

  def deploy_notebook_sync(notebook) do
    app_spec = Livebook.Apps.NotebookAppSpec.new(notebook)

    deployer_pid = Livebook.Apps.Deployer.local_deployer()
    ref = Livebook.Apps.Deployer.deploy_monitor(deployer_pid, app_spec)

    receive do
      {:deploy_result, ^ref, {:ok, pid}} ->
        Process.demonitor(ref, [:flush])

        on_exit(fn ->
          if Process.alive?(pid) do
            Livebook.App.close(pid)
          end
        end)

        pid
    end
  end

  def deploy_app(slug, team, org, deployment_group, tmp_dir, node) do
    app_path = Path.join(tmp_dir, "#{slug}.livemd")

    source =
      stamp_notebook(app_path, """
      <!-- livebook:{"app_settings":{"access_type":"public","slug":"#{slug}"},"hub_id":"#{team.id}","deployment_group_id":"#{deployment_group.id}"} -->

      # LivebookApp:#{slug}

      ```elixir
      IO.puts("Hi")
      ```
      """)

    files_dir = Livebook.FileSystem.File.local(tmp_dir)

    {:ok, %Livebook.Teams.AppDeployment{file: zip_content} = app_deployment} =
      Livebook.Teams.AppDeployment.new(source, files_dir)

    secret_key = Livebook.Teams.derive_key(team.teams_key)
    encrypted_content = Livebook.Teams.encrypt(zip_content, secret_key)

    app_deployment =
      TeamsRPC.upload_app_deployment(
        node,
        org,
        deployment_group,
        app_deployment,
        encrypted_content,
        # broadcast?
        true
      )

    app_deployment_id = to_string(app_deployment.id)
    assert_receive {:app_deployment_started, %{id: ^app_deployment_id} = app_deployment}

    app_deployment
  end

  def wait_livebook_app_start(slug) do
    assert_receive {:app_created, %{pid: pid, slug: ^slug}}, 5_000

    assert_receive {:app_updated,
                    %{
                      slug: ^slug,
                      sessions: [%{app_status: %{execution: :executed, lifecycle: :active}}]
                    }},
                   5_000

    on_exit(fn ->
      if Process.alive?(pid) do
        Livebook.App.close(pid)
      end
    end)

    pid
  end

  def stamp_notebook(app_path, %Livebook.Notebook{} = notebook) do
    {source, []} = Livebook.LiveMarkdown.notebook_to_livemd(notebook)
    File.write!(app_path, source)

    source
  end

  def stamp_notebook(app_path, source) when is_binary(source) do
    {notebook, %{warnings: []}} = Livebook.LiveMarkdown.notebook_from_livemd(source)
    stamp_notebook(app_path, notebook)
  end
end

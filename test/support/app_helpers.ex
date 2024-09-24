defmodule Livebook.AppHelpers do
  def deploy_notebook_sync(notebook) do
    app_spec = Livebook.Apps.NotebookAppSpec.new(notebook)

    deployer_pid = Livebook.Apps.Deployer.local_deployer()
    ref = Livebook.Apps.Deployer.deploy_monitor(deployer_pid, app_spec)

    receive do
      {:deploy_result, ^ref, {:ok, pid}} ->
        Process.demonitor(ref, [:flush])

        ExUnit.Callbacks.on_exit(fn ->
          if Process.alive?(pid) do
            Livebook.App.close(pid)
          end
        end)

        pid
    end
  end
end

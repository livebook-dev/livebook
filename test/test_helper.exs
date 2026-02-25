# We setup Pythonx in the current OS process, so we can test Python
# code evaluation and intellisense. Testing pyproject.toml evaluation
# is tricky because it requires a separate VM, so we only rely on the
# LV integration tests.

# TODO: Update `pythonx` to support Nix, downloaded binaries doesn't work
nix? = System.find_executable("nix") != nil

if not nix? do
  ExUnit.CaptureIO.capture_io(fn ->
    Pythonx.uv_init("""
    [project]
    name = "project"
    version = "0.0.0"
    requires-python = "==3.13.*"
    dependencies = []
    """)
  end)
end

# Start manager on the current node and configure it not to terminate
# automatically, so that we can use it to start runtime servers
# explicitly
Livebook.Runtime.ErlDist.NodeManager.start(
  auto_termination: false,
  unload_modules_on_termination: false,
  capture_orphan_logs: false
)

# Use the embedded runtime in tests by default, so they are cheaper
# to run. Other runtimes can be tested by setting them explicitly
Application.put_env(:livebook, :default_runtime, Livebook.Runtime.Embedded.new())
Application.put_env(:livebook, :default_app_runtime, Livebook.Runtime.Embedded.new())

Application.put_env(:livebook, :runtime_modules, [
  Livebook.Runtime.Standalone,
  Livebook.Runtime.Attached,
  Livebook.Runtime.Embedded,
  Livebook.Runtime.Fly,
  Livebook.Runtime.K8s
])

defmodule Livebook.Runtime.Embedded.Packages do
  def list() do
    [
      %{
        dependency: %{dep: {:req, "~> 0.5.0"}, config: []},
        description: "Req is a batteries-included HTTP client for Elixir.",
        name: "req",
        url: "https://hex.pm/packages/req",
        version: "0.5.0"
      }
    ]
  end
end

# Enable dependency search for the embedded runtime
Application.put_env(:livebook, Livebook.Runtime.Embedded,
  load_packages: {Livebook.Runtime.Embedded.Packages, :list, []}
)

# Disable autosaving
Livebook.Storage.insert(:settings, "global", autosave_path: nil)

# Always use the same secret key in tests
secret_key =
  "5ji8DpnX761QAWXZwSl-2Y-mdW4yTcMimdOJ8SSxCh44wFE0jEbGBUf-VydKwnTLzBiAUedQKs3X_q1j_3lgrw"

personal_hub = Livebook.Hubs.fetch_hub!(Livebook.Hubs.Personal.id())
Livebook.Hubs.Personal.update_hub(personal_hub, %{secret_key: secret_key})

# Always set the same offline team hub in tests
Livebook.HubHelpers.set_offline_hub()

# Compile anything pending on TeamsServer
Livebook.TeamsServer.setup()

windows? = match?({:win32, _}, :os.type())
without_docs? = match?({:error, _}, Code.fetch_docs(:gen_server))
git_ssh_key? = System.get_env("TEST_GIT_SSH_KEY") != nil
fly_api_token? = System.get_env("TEST_FLY_API_TOKEN") != nil

ExUnit.start(
  assert_receive_timeout: if(windows?, do: 5_000, else: 1_500),
  exclude: [
    python: nix?,
    git: not git_ssh_key?,
    fly: not fly_api_token?,
    teams_integration: not Livebook.TeamsServer.available?(),
    unix: windows?,
    k8s: true,
    erl_docs: without_docs?
  ]
)

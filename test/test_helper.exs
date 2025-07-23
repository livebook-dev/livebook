# Start manager on the current node and configure it not to terminate
# automatically, so that we can use it to start runtime servers
# explicitly
Livebook.Runtime.ErlDist.NodeManager.start(
  auto_termination: false,
  unload_modules_on_termination: false
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

erl_docs_exclude =
  if match?({:error, _}, Code.fetch_docs(:gen_server)) do
    [:erl_docs]
  else
    []
  end

windows_exclude = if windows?, do: [:unix], else: []

teams_exclude =
  if Livebook.TeamsServer.available?() do
    []
  else
    [:teams_integration]
  end

fly_exclude = if System.get_env("TEST_FLY_API_TOKEN"), do: [], else: [:fly]

ExUnit.start(
  assert_receive_timeout: if(windows?, do: 5_000, else: 1_500),
  exclude: erl_docs_exclude ++ windows_exclude ++ teams_exclude ++ fly_exclude ++ [:k8s]
)

# Start manager on the current node and configure it not to
# terminate automatically, so there is no race condition
# when starting/stopping Embedded runtimes in parallel
Livebook.Runtime.ErlDist.NodeManager.start(
  auto_termination: false,
  unload_modules_on_termination: false
)

# Use the embedded runtime in tests by default, so they are
# cheaper to run. Other runtimes can be tested by starting
# and setting them explicitly
Application.put_env(:livebook, :default_runtime, Livebook.Runtime.Embedded.new())
Application.put_env(:livebook, :default_app_runtime, Livebook.Runtime.Embedded.new())

Application.put_env(:livebook, :runtime_modules, [
  Livebook.Runtime.ElixirStandalone,
  Livebook.Runtime.Attached,
  Livebook.Runtime.Embedded
])

defmodule Livebook.Runtime.Embedded.Packages do
  def list() do
    [
      %{
        dependency: %{dep: {:jason, "~> 1.3.0"}, config: []},
        description: "A blazing fast JSON parser and generator in pure Elixir",
        name: "jason",
        url: "https://hex.pm/packages/jason",
        version: "1.3.0"
      }
    ]
  end
end

# Enable dependency saerch for the embedded runtime
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

erl_docs_available? = Code.fetch_docs(:gen_server) != {:error, :chunk_not_found}

windows? = match?({:win32, _}, :os.type())

ExUnit.start(
  assert_receive_timeout: if(windows?, do: 2_500, else: 1_500),
  exclude: [
    erl_docs: erl_docs_available?,
    teams_integration: not Livebook.TeamsServer.available?()
  ]
)

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

Application.put_env(:livebook, :runtime_modules, [
  Livebook.Runtime.ElixirStandalone,
  Livebook.Runtime.MixStandalone,
  Livebook.Runtime.Attached,
  Livebook.Runtime.Embedded
])

defmodule Livebook.Runtime.Embedded.Packages do
  def list() do
    [
      %{
        dependency: {:jason, "~> 1.3.0"},
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
Livebook.Storage.current().insert(:settings, "global", autosave_path: nil)

erl_docs_available? = Code.fetch_docs(:gen_server) != {:error, :chunk_not_found}

exclude = []
exclude = if erl_docs_available?, do: exclude, else: Keyword.put(exclude, :erl_docs, true)

ExUnit.start(assert_receive_timeout: 1_500, exclude: exclude)

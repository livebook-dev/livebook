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

# Disable autosaving
Livebook.Storage.current().insert(:settings, "global", autosave_path: nil)

erl_docs_available? = Code.fetch_docs(:gen_server) != {:error, :chunk_not_found}

exclude = []
exclude = if erl_docs_available?, do: exclude, else: Keyword.put(exclude, :erl_docs, true)

ExUnit.start(assert_receive_timeout: 1_000, exclude: exclude)

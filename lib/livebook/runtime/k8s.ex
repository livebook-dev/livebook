defmodule Livebook.Runtime.K8s do
  # A runtime backed by a Kubernetes Pod managed by Livebook.
  #
  # This runtime uses a Livebook-managed Elixir node, similarly to
  # the Standalone runtime, however it runs on a temporary Kubernetes Pod
  # machine. The machine is configured to automatically shutdown
  # as soon as the runtime is disconnected.
  #
  # Note: this runtime requires `kubectl` executable to be available
  # in the system.
  #
  # ## Communication
  #
  # The machine runs the Livebook Docker image and we configure it to
  # invoke the start_runtime.exs script, by setting LIVEBOOK_RUNTIME.
  # This environment variable also includes encoded information passed
  # from the parent. Once the Elixir node starts on the machine, it
  # waits for the parent to connect and finish the initialization.
  #
  # Now, we want to establish a distribution connection from the local
  # Livebook node to the node on the Kubernetes Pod. We could reach
  # the node directly, by requiring the user to set up WireGuard.
  # However, that would require the user to install WireGuard and go
  # through a few configuration steps. Instead, we use kubectl proxy
  # feature and only require kubectl to be installed.
  #
  # With kubectl proxy, we proxy a local port to the the distribution
  # port of the Kubernets node. Then, in our EPMD module (`Livebook.EPMD`)
  # we special case those nodes in two ways: (1) we infer the
  # distribution port from the node name; (2) we resolve the node
  # address to loopback, ignoring its hostname.
  #
  # ### Distribution protocol
  #
  # Usually, nodes need to be configured to use the same distribution
  # protocol (`-proto_dist`). We configure the Kubernetes node to use IPv6
  # distribution (`-proto_dist inet6_tcp`). However, depending whether
  # the local node runs IPv4 or IPv6 distribution, we configure the
  # kubectl proxy to bind to a IPv4 or IPv6 loopback respectively. The
  # proxy always communicates with the Kubernetes Pod over IPv6, as
  # is the case with all internal networking. Consequently, regardless
  # of the protocol used by the local node, the remote node perceives
  # it as IPv6.
  #
  # Sidenote, a node using IPv6 distribution may accept connections
  # from a node using IPv4, depending on the `:kernel` application
  # configuration `inet_dist_listen_options` -> `ipv6_v6only`, which
  # has OS-specific value. However, we don't rely on this here.

  defstruct [:config, :node, :server_pid]

  use GenServer, restart: :temporary

  require Logger

  @impl true
  def init({runtime, caller}) do
    state = %{primary_ref: nil, proxy_port: nil}
    {:ok, state, {:continue, {:init, runtime, caller}}}
  end

  @impl true
  def handle_continue({:init, _runtime, _caller}, state) do
    {:noreply, state}
  end
end

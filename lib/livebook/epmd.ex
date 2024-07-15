defmodule Livebook.EPMD do
  # A custom EPMD module used to bypass the epmd OS daemon on Livebook.
  #
  # We also use it for the Fly runtime, such that we connect to the
  # remote node via a local proxy port.

  # From Erlang/OTP 23+
  @epmd_dist_version 6

  @doc """
  Gets a random child node name.
  """
  def random_child_node() do
    String.to_atom(Livebook.EPMD.NodePool.get_name())
  end

  @doc """
  Updates the port information for the given node.
  """
  def update_child_node(node, port) do
    Livebook.EPMD.NodePool.update_name(Atom.to_string(node), port)
  end

  @doc """
  Returns the Livebook distribution port, if Livebook.EPMD is running, otherwise 0.
  """
  def dist_port do
    :persistent_term.get(:livebook_dist_port, 0)
  end

  # Custom EPMD callbacks

  # Custom callback to register our current node port.
  def register_node(name, port), do: register_node(name, port, :inet)

  def register_node(name, port, family) do
    :persistent_term.put(:livebook_dist_port, port)

    case :erl_epmd.register_node(name, port, family) do
      {:ok, creation} -> {:ok, creation}
      {:error, :already_registered} -> {:error, :already_registered}
      # If registration fails because EPMD is not running, we ignore
      # that, because we do not rely on EPMD
      _ -> {:ok, -1}
    end
  end

  # Custom callback that accesses the parent information.
  def port_please(name, host), do: port_please(name, host, :infinity)

  def port_please(~c"remote_runtime_" ++ port, _host, _timeout) do
    # The node name includes the local port proxied to a remote machine
    port = List.to_integer(port)
    {:port, port, @epmd_dist_version}
  end

  def port_please(name, host, timeout) do
    :erl_epmd.port_please(name, host, timeout)
  end

  # Custom callback for resolving remote runtime node domain, such as
  # Fly .internal, to loopback, because we communicate via a local
  # proxied port
  def address_please(~c"remote_runtime_" ++ _, _host, address_family) do
    case address_family do
      :inet -> {:ok, {127, 0, 0, 1}}
      :inet6 -> {:ok, {0, 0, 0, 0, 0, 0, 0, 1}}
    end
  end

  def address_please(name, host, address_family) do
    :erl_epmd.address_please(name, host, address_family)
  end

  # Default EPMD callbacks

  defdelegate start_link(), to: :erl_epmd
  defdelegate listen_port_please(name, host), to: :erl_epmd
  defdelegate names(host_name), to: :erl_epmd
end

{:module, _name, binary, _result} =
  defmodule Livebook.EPMD do
    # A custom EPMD module used to bypass the epmd OS daemon
    # on both Livebook and the runtimes.

    # From Erlang/OTP 23+
    @epmd_dist_version 6

    @doc """
    Returns the parent Livebook name prefix (as a charlist),
    full node name (as an atom), and port (as an integer but may be zero).
    Or nil if there is no parent node.
    """
    def parent_info do
      :persistent_term.get(:livebook_parent, nil)
    end

    @doc """
    Returns the Livebook distribution port, if Livebook.EPMD is running, otherwise 0.
    """
    def dist_port do
      :persistent_term.get(:livebook_dist_port, 0)
    end

    # Custom EPMD callbacks

    # Custom callback that registers the parent information.
    # We read this information when trying to connect to the parent.
    def start_link() do
      with {:ok, [[node, port]]} <- :init.get_argument(:livebook_parent) do
        [name | _] = :string.split(node, ~c"@")
        :persistent_term.put(:livebook_parent, {name, List.to_atom(node), List.to_integer(port)})
      end

      :erl_epmd.start_link()
    end

    # Custom callback to register our current node port.
    def register_node(name, port), do: register_node(name, port, :inet)

    def register_node(name, port, family) do
      :persistent_term.put(:livebook_dist_port, port)
      :erl_epmd.register_node(name, port, family)
    end

    # Custom callback that accesses the parent information.
    def port_please(name, host), do: port_please(name, host, :infinity)

    def port_please(name, host, timeout) do
      case parent_info() do
        {^name, _node, port} when port != 0 -> {:port, port, @epmd_dist_version}
        _ -> :erl_epmd.port_please(name, host, timeout)
      end
    end

    # Default EPMD callbacks

    defdelegate listen_port_please(name, host), to: :erl_epmd
    defdelegate names(host_name), to: :erl_epmd
    defdelegate address_please(name, host, address_family), to: :erl_epmd
  end

File.mkdir_p!("priv/epmd/ebin")
File.write!("priv/epmd/ebin/Elixir.Livebook.EPMD.beam", binary)

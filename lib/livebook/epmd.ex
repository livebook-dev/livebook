{:module, _name, binary, _result} =
  defmodule Livebook.EPMD do
    # A custom EPMD module used to bypass the epmd OS daemon
    # on both Livebook and the runtimes.

    # From Erlang/OTP 23+
    @epmd_dist_version 6
    @external_resource "priv/epmd/Elixir.Livebook.EPMD.beam"

    @doc """
    Gets a random child node name.
    """
    def random_child_node do
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

    # Custom callback that registers the parent information.
    # We read this information when trying to connect to the parent.
    def start_link() do
      with {:ok, [[node, port]]} <- :init.get_argument(:livebook_parent) do
        [name, host] = :string.split(node, ~c"@")

        :persistent_term.put(
          :livebook_parent,
          {name, host, List.to_atom(node), List.to_integer(port)}
        )
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
      case livebook_port(name) do
        0 -> :erl_epmd.port_please(name, host, timeout)
        port -> {:port, port, @epmd_dist_version}
      end
    end

    # If we are running inside a Livebook Runtime,
    # we should be able to reach the parent directly
    # or reach siblings through the parent.
    defp livebook_port(name) do
      case :persistent_term.get(:livebook_parent, nil) do
        {parent_name, parent_host, parent_node, parent_port} ->
          case match_name(name, parent_name) do
            :parent -> parent_port
            :sibling -> sibling_port(parent_node, name, parent_host)
            :none -> 0
          end

        _ ->
          0
      end
    end

    defp match_name([x | name], [x | parent_name]), do: match_name(name, parent_name)
    defp match_name([?-, ?- | _name], _parent), do: :sibling
    defp match_name([], []), do: :parent
    defp match_name(_name, _parent), do: :none

    defp sibling_port(parent_node, name, host) do
      :gen_server.call(
        {Livebook.EPMD.NodePool, parent_node},
        {:get_port, :erlang.list_to_binary(name ++ [?@] ++ host)},
        5000
      )
    catch
      _, _ -> 0
    end

    # Default EPMD callbacks

    defdelegate listen_port_please(name, host), to: :erl_epmd
    defdelegate names(host_name), to: :erl_epmd
    defdelegate address_please(name, host, address_family), to: :erl_epmd
  end

File.mkdir_p!("priv/epmd")
File.write!("priv/epmd/Elixir.Livebook.EPMD.beam", binary)

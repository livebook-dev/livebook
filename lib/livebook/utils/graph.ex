defmodule Livebook.Utils.Graph do
  @moduledoc false

  @typedoc """
  A bottom-up graph representation encoded as a map
  of child-to-parent entries.
  """
  @type t() :: %{node_id => node_id | nil}

  @type t(node_id) :: %{node_id => node_id | nil}

  @type node_id :: term()

  @doc """
  Finds a path between nodes `from_id` and `to_id`.

  If the path exists, a top-down list of nodes is
  returned including the extreme nodes. Otherwise,
  an empty list is returned.
  """
  @spec find_path(t(), node_id(), node_id()) :: list(node_id())
  def find_path(graph, from_id, to_id) do
    find_path(graph, from_id, to_id, [])
  end

  defp find_path(_graph, to_id, to_id, path), do: [to_id | path]
  defp find_path(_graph, nil, _to_id, _path), do: []

  defp find_path(graph, from_id, to_id, path),
    do: find_path(graph, graph[from_id], to_id, [from_id | path])

  @doc """
  Finds grpah leave nodes, that is, nodes with
  no children.
  """
  @spec leaves(t()) :: list(node_id())
  def leaves(graph) do
    children = MapSet.new(graph, fn {key, _} -> key end)
    parents = MapSet.new(graph, fn {_, value} -> value end)
    MapSet.difference(children, parents) |> MapSet.to_list()
  end
end

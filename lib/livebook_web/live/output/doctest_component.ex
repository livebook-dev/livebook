defmodule LivebookWeb.Output.DoctestComponent do
  use LivebookWeb, :live_component

  @impl true
  def update(assigns, socket) do
    cell_id = assigns.cell_id

    doctests_result =
      for result <- assigns.results, reduce: %{} do
        acc -> Map.put(acc, result.doctest_line, (result.state == :success && true) || false)
      end

    socket = push_event(socket, "doctests_result:#{cell_id}", doctests_result)
    {:ok, assign(socket, assigns)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={"virtualized-doctest-#{@id}"} />
    """
  end
end

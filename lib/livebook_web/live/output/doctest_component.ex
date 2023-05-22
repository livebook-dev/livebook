defmodule LivebookWeb.Output.DoctestComponent do
  use LivebookWeb, :live_component

  @impl true
  def update(assigns, socket) do
    cell_id = assigns.cell_id

    %{doctest_line: doctest_line, state: state} = assigns.result

    socket =
      case state do
        :evaluating ->
          push_event(socket, "evaluating_doctest:#{cell_id}", %{line: doctest_line})

        :success ->
          push_event(socket, "success_doctest:#{cell_id}", %{line: doctest_line})

        :failed ->
          push_event(socket, "failed_doctest:#{cell_id}", %{line: doctest_line})
      end

    {:ok, assign(socket, assigns)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={"virtualized-doctest-#{@id}"} />
    """
  end
end

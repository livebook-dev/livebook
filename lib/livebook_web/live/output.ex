defmodule LivebookWeb.Output do
  use Phoenix.Component

  @doc """
  Renders a list of cell outputs.
  """
  def outputs(assigns) do
    ~H"""
    <div class="flex flex-col space-y-2">
      <%= for {output_views, standalone?} <- group_output_views(@output_views) do %>
        <div class={"flex flex-col #{if not standalone?, do: "rounded-lg border border-gray-200 divide-y divide-gray-200"}"}>
          <%= for output_view <- output_views do %>
            <div class={"max-w-full py-4 #{if not standalone?, do: "px-4"}"}>
              <.live_component module={LivebookWeb.OutputComponent}
                id={output_view.id}
                output={output_view.output}
                session_id={@session_id}
                runtime={@runtime}
                cell_validity_status={@cell_validity_status}
                input_values={@input_values} />
            </div>
          <% end %>
        </div>
      <% end %>
    </div>
    """
  end

  defp group_output_views(output_views) do
    output_views = Enum.reject(output_views, &match?(%{output: :ignored}, &1))
    group_output_views(output_views, [])
  end

  defp group_output_views([], groups), do: groups

  defp group_output_views([view | views], []) do
    group_output_views(views, [{[view], standalone?(view.output)}])
  end

  defp group_output_views([view | views], [{group_views, group_standalone?} | groups]) do
    case standalone?(view.output) do
      ^group_standalone? ->
        group_output_views(views, [{[view | group_views], group_standalone?} | groups])

      standalone? ->
        group_output_views(
          views,
          [{[view], standalone?}, {group_views, group_standalone?} | groups]
        )
    end
  end

  defp standalone?(:ignored), do: false
  defp standalone?(text) when is_binary(text), do: false
  defp standalone?({:text, _text}), do: false
  defp standalone?({:error, _message, _type}), do: false
  defp standalone?(_output), do: true
end

defmodule LivebookWeb.SessionLive.RuntimeHelpers do
  use Phoenix.Component

  @doc """
  Displays an info text if `@module` is the default runtime.
  """
  def default_runtime_note(assigns) do
    ~H"""
    <%= if default_runtime_module?(@module) do %>
      <p class="text-gray-600 text-sm">
        Note: This is the <span class="font-semibold">default runtime</span> and starts
        automatically as soon as you evaluate the first cell.
      </p>
    <% end %>
    """
  end

  defp default_runtime_module?(module) do
    {default_module, _args} = Livebook.Config.default_runtime()
    default_module == module
  end
end

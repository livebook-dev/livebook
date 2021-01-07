defmodule LiveBookWeb.HomeLive do
  use LiveBookWeb, :live_view

  @impl true
  def render(assigns) do
    ~L"""
    <div class="container p-4 flex justify-center">
      <h1 class="text-2xl">Welcome to LiveBook</h1>
    </div>
    """
  end
end

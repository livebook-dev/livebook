defmodule LivebookWeb.AppHelpers do
  use LivebookWeb, :html

  @doc """
  Renders app status with indicator.
  """
  attr :status, :atom, required: true

  def app_status(%{status: :booting} = assigns) do
    ~H"""
    <.app_status_indicator text="Booting" variant={:progressing} />
    """
  end

  def app_status(%{status: :running} = assigns) do
    ~H"""
    <.app_status_indicator text="Running" variant={:success} />
    """
  end

  def app_status(%{status: :error} = assigns) do
    ~H"""
    <.app_status_indicator text="Error" variant={:error} />
    """
  end

  def app_status(%{status: :shutting_down} = assigns) do
    ~H"""
    <.app_status_indicator text="Shutting down" variant={:inactive} />
    """
  end

  def app_status(%{status: :stopped} = assigns) do
    ~H"""
    <.app_status_indicator text="Stopped" variant={:inactive} />
    """
  end

  defp app_status_indicator(assigns) do
    ~H"""
    <div class="flex items-center space-x-2">
      <div><%= @text %></div>
      <.status_indicator variant={@variant} />
    </div>
    """
  end
end

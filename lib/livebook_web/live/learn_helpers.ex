defmodule LivebookWeb.LearnHelpers do
  use Phoenix.Component

  use LivebookWeb, :verified_routes

  @doc """
  Renders an learn notebook card.
  """
  attr :notebook_info, :map, required: true

  def notebook_card(assigns) do
    ~H"""
    <.link
      navigate={~p"/learn/notebooks/#{@notebook_info.slug}"}
      class="flex flex-col border-2 border-gray-100 hover:border-gray-200 rounded-2xl"
    >
      <div class="flex items-center justify-center p-6 border-b-2 border-gray-100 rounded-t-2xl h-[150px]">
        <img
          src={learn_img_src(@notebook_info.details.cover)}
          class="max-h-full max-w-[75%]"
          alt={"#{@notebook_info.title} logo"}
        />
      </div>
      <div class="px-6 py-4 bg-gray-100 rounded-b-2xl grow">
        <span class="text-gray-800 font-semibold"><%= @notebook_info.title %></span>
        <p class="mt-2 text-sm text-gray-600">
          <%= @notebook_info.details.description %>
        </p>
      </div>
    </.link>
    """
  end

  @doc """
  Resolves the given image source into a URL.
  """
  @spec learn_img_src(Livebook.Notebook.Learn.image_source()) :: String.t()
  def learn_img_src({:url, url}), do: url
  def learn_img_src({:static, filename}), do: ~p"/images/#{filename}"
end

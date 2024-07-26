defmodule LivebookWeb.NotebookComponents do
  use LivebookWeb, :html

  @doc """
  Renders a learn notebook card.
  """
  attr :notebook_info, :map, required: true

  def learn_notebook_card(assigns) do
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

  @doc """
  Renders an icon for the given cell.
  """
  attr :cell_type, :atom, required: true
  attr :language, :atom, default: nil

  def cell_icon(assigns)

  def cell_icon(%{cell_type: :code, language: :elixir} = assigns) do
    ~H"""
    <div class="flex w-6 h-6 bg-purple-100 rounded items-center justify-center">
      <svg width="11" height="15" viewBox="0 0 11 15" fill="none" xmlns="http://www.w3.org/2000/svg">
        <path
          d="M5.7784 3.58083C7.4569 5.87527 9.67878 5.70652 10.0618 9.04833C10.1147 12.9425 8.03684
        14.27 6.55353 14.6441C4.02227 15.3635 1.7644 14.2813 0.875648 11.8316C-0.83154 7.89408 2.36684
        1.41746 4.42502 0.0668945C4.60193 1.32119 5.05745 2.51995 5.75815 3.57521L5.7784 3.58083Z"
          fill="#663299"
        />
      </svg>
    </div>
    """
  end

  def cell_icon(%{cell_type: :code, language: :erlang} = assigns) do
    ~H"""
    <div class="flex w-6 h-6 bg-red-100 rounded items-center justify-center">
      <svg width="18" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 15 10">
        <g fill="#a90533">
          <path d="M2.4 10A7.7 7.7 0 0 1 .5 4.8c0-2 .6-3.6 1.6-4.8H0v10ZM13 10c.5-.6 1-1.2 1.4-2l-2.3-1.2c-.8 1.4-2 2.6-3.6 2.6-2.3 0-3.2-2-3.2-4.8H14V4c0-1.6-.3-3-1-4H15v10h-2Zm0 0" />
          <path d="M5.5 2.3c.1-1.2 1-2 2.1-2s1.9.8 2 2Zm0 0" />
        </g>
      </svg>
    </div>
    """
  end

  def cell_icon(%{cell_type: :markdown} = assigns) do
    ~H"""
    <div class="flex w-6 h-6 bg-blue-100 rounded items-center justify-center">
      <svg width="16" height="14" viewBox="0 0 16 14" fill="none" xmlns="http://www.w3.org/2000/svg">
        <path
          d="M1.25 0.25H14.75C14.9489 0.25 15.1397 0.329018 15.2803 0.46967C15.421 0.610322 15.5 0.801088
        15.5 1V13C15.5 13.1989 15.421 13.3897 15.2803 13.5303C15.1397 13.671 14.9489 13.75 14.75 13.75H1.25C1.05109
        13.75 0.860322 13.671 0.71967 13.5303C0.579018 13.3897 0.5 13.1989 0.5 13V1C0.5 0.801088 0.579018 0.610322
        0.71967 0.46967C0.860322 0.329018 1.05109 0.25 1.25 0.25ZM4.25 9.625V6.625L5.75 8.125L7.25
        6.625V9.625H8.75V4.375H7.25L5.75 5.875L4.25 4.375H2.75V9.625H4.25ZM12.5 7.375V4.375H11V7.375H9.5L11.75
        9.625L14 7.375H12.5Z"
          fill="#3E64FF"
        />
      </svg>
    </div>
    """
  end

  def cell_icon(%{cell_type: :smart} = assigns) do
    ~H"""
    <div class="flex w-6 h-6 bg-red-100 rounded items-center justify-center">
      <.remix_icon icon="flashlight-line text-red-900" />
    </div>
    """
  end
end

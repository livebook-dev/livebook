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
        <span class="text-gray-800 font-semibold">{@notebook_info.title}</span>
        <p class="mt-2 text-sm text-gray-600">
          {@notebook_info.details.description}
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

  def cell_icon(%{cell_type: :code} = assigns) do
    ~H"""
    <div class="w-6 h-6 p-1 rounded flex items-center justify-center bg-gray-100">
      <.language_icon language={Atom.to_string(@language)} class="w-full h-full text-gray-600" />
    </div>
    """
  end

  def cell_icon(%{cell_type: :markdown} = assigns) do
    ~H"""
    <div class="w-6 h-6 p-1 rounded flex items-center justify-center bg-gray-100">
      <.language_icon language="markdown" class="w-full h-full text-gray-600" />
    </div>
    """
  end

  def cell_icon(%{cell_type: :smart} = assigns) do
    ~H"""
    <div class="flex w-6 h-6 p-1 rounded items-center justify-center bg-gray-100">
      <.remix_icon icon="flashlight-line text-gray-600" />
    </div>
    """
  end

  @doc """
  Renders an icon for the given language.

  The icons are adapted from https://github.com/material-extensions/vscode-material-icon-theme.
  """
  attr :language, :string, required: true
  attr :class, :string, default: nil

  def language_icon(%{language: "elixir"} = assigns) do
    ~H"""
    <svg class={@class} viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
      <path
        fill="currentColor"
        d="M12.173 22.681c-3.86 0-6.99-3.64-6.99-8.13 0-3.678 2.773-8.172 4.916-10.91 1.014-1.296 2.93-2.322 2.93-2.322s-.982 5.239 1.683 7.319c2.366 1.847 4.106 4.25 4.106 6.363 0 4.232-2.784 7.68-6.645 7.68"
      />
    </svg>
    """
  end

  def language_icon(%{language: "erlang"} = assigns) do
    ~H"""
    <svg class={@class} viewBox="0 0 30 30" xmlns="http://www.w3.org/2000/svg">
      <path
        fill="currentColor"
        d="M5.207 4.33q-.072.075-.143.153Q1.5 8.476 1.5 15.33c0 4.418 1.155 7.862 3.459 10.34h19.415c2.553-1.152 4.127-3.43 4.127-3.43l-3.147-2.52L23.9 21.1c-.867.773-.845.931-2.315 1.78-1.495.674-3.04.966-4.634.966-2.515 0-4.423-.909-5.723-2.059-1.286-1.15-1.985-4.511-2.096-6.68l17.458.067-.183-1.472s-.847-7.129-2.541-9.372zm8.76.846c1.565 0 3.22.535 3.961 1.471.74.937.931 1.667.973 3.524H9.11c.112-1.955.436-2.81 1.373-3.698.936-.887 2.03-1.297 3.484-1.297"
      />
    </svg>
    """
  end

  def language_icon(%{language: "markdown"} = assigns) do
    ~H"""
    <svg class={@class} viewBox="0 0 32 32" xmlns="http://www.w3.org/2000/svg">
      <path
        fill="currentColor"
        d="m14 10-4 3.5L6 10H4v12h4v-6l2 2 2-2v6h4V10zm12 6v-6h-4v6h-4l6 8 6-8z"
      />
    </svg>
    """
  end

  def language_icon(%{language: "python"} = assigns) do
    ~H"""
    <svg class={@class} viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
      <path
        fill="currentColor"
        d="M9.86 2A2.86 2.86 0 0 0 7 4.86v1.68h4.29c.39 0 .71.57.71.96H4.86A2.86 2.86 0 0 0 2 10.36v3.781a2.86 2.86 0 0 0 2.86 2.86h1.18v-2.68a2.85 2.85 0 0 1 2.85-2.86h5.25c1.58 0 2.86-1.271 2.86-2.851V4.86A2.86 2.86 0 0 0 14.14 2zm-.72 1.61c.4 0 .72.12.72.71s-.32.891-.72.891c-.39 0-.71-.3-.71-.89s.32-.711.71-.711"
      />
      <path
        fill="currentColor"
        d="M17.959 7v2.68a2.85 2.85 0 0 1-2.85 2.859H9.86A2.85 2.85 0 0 0 7 15.389v3.75a2.86 2.86 0 0 0 2.86 2.86h4.28A2.86 2.86 0 0 0 17 19.14v-1.68h-4.291c-.39 0-.709-.57-.709-.96h7.14A2.86 2.86 0 0 0 22 13.64V9.86A2.86 2.86 0 0 0 19.14 7zM8.32 11.513l-.004.004.038-.004zm6.54 7.276c.39 0 .71.3.71.89a.71.71 0 0 1-.71.71c-.4 0-.72-.12-.72-.71s.32-.89.72-.89"
      />
    </svg>
    """
  end

  def language_icon(%{language: "pyproject.toml"} = assigns) do
    ~H"""
    <svg class={@class} xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16">
      <path fill="currentColor" d="M4 6V4h8v2H9v7H7V6z" />
      <path fill="currentColor" d="M4 1v1H2v12h2v1H1V1zm8 0v1h2v12h-2v1h3V1z" />
    </svg>
    """
  end
end

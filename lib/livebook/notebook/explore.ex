defmodule Livebook.Notebook.Explore.Utils do
  @moduledoc false

  @doc """
  Defines a module attribute `attr` with notebook info.
  """
  defmacro defnotebook(attr, props) do
    quote bind_quoted: [attr: attr, props: props] do
      {path, notebook_info} = Livebook.Notebook.Explore.Utils.fetch_notebook!(attr, props)

      @external_resource path

      Module.put_attribute(__MODULE__, attr, notebook_info)
    end
  end

  def fetch_notebook!(attr, props) do
    name = Atom.to_string(attr)
    path = Path.join([__DIR__, "explore", name <> ".livemd"])

    markdown = File.read!(path)
    # Parse the file to ensure no warnings and read the title.
    # However, in the info we keep just the file contents to save on memory.
    {notebook, []} = Livebook.LiveMarkdown.Import.notebook_from_markdown(markdown)

    notebook_info = %{
      slug: String.replace(name, "_", "-"),
      livemd: markdown,
      title: notebook.name,
      description: Keyword.fetch!(props, :description),
      image_url: Keyword.fetch!(props, :image_url)
    }

    {path, notebook_info}
  end
end

defmodule Livebook.Notebook.Explore do
  @moduledoc false

  defmodule NotFoundError do
    @moduledoc false

    defexception [:slug, plug_status: 404]

    def message(%{slug: slug}) do
      "could not find an example notebook matching #{inspect(slug)}"
    end
  end

  import Livebook.Notebook.Explore.Utils

  defnotebook(:intro_to_livebook,
    description: "Get to know Livebook, see how it works and explore its features.",
    image_url: "/images/logo.png"
  )

  defnotebook(:elixir_and_livebook,
    description: "Learn how to use some of Elixir and Livebook unique features together.",
    image_url: "/images/live-elixir.png"
  )

  defnotebook(:intro_to_elixir,
    description: "New to Elixir? Learn about the language and its core concepts.",
    image_url: "/images/elixir.png"
  )

  defnotebook(:intro_to_nx,
    description:
      "Enter numerical Elixir, experience the power of multi-dimensional arrays of numbers.",
    image_url: "/images/nx.png"
  )

  defnotebook(:intro_to_axon,
    description: "Build Neural Networks in Elixir using a high-level, composable API.",
    image_url: "/images/axon.png"
  )

  defnotebook(:intro_to_vega_lite,
    description: "Learn how to quickly create numerous plots for your data.",
    image_url: "/images/vega_lite.png"
  )

  @type notebook_info :: %{
          slug: String.t(),
          livemd: String.t(),
          title: String.t(),
          description: String.t(),
          image_url: String.t()
        }

  @doc """
  Returns a list of example notebooks with metadata.
  """
  @spec notebook_infos() :: list(notebook_info())
  def notebook_infos() do
    [
      @intro_to_livebook,
      @elixir_and_livebook
      # @intro_to_elixir, @intro_to_nx, @intro_to_axon, @intro_to_vega_lite
    ]
  end

  @doc """
  Finds explore notebook by slug and returns the parsed data structure.
  """
  @spec notebook_by_slug!(String.t()) :: Livebook.Notebook.t()
  def notebook_by_slug!(slug) do
    notebook_infos()
    |> Enum.find(&(&1.slug == slug))
    |> case do
      nil ->
        raise NotFoundError, slug: slug

      notebook_info ->
        {notebook, []} = Livebook.LiveMarkdown.Import.notebook_from_markdown(notebook_info.livemd)
        notebook
    end
  end
end

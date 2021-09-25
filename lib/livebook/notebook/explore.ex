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

    images =
      props
      |> Keyword.get(:image_names, [])
      |> Map.new(fn image_name ->
        path = Path.join([__DIR__, "explore", "images", image_name])
        content = File.read!(path)
        {image_name, content}
      end)

    notebook_info = %{
      slug: String.replace(name, "_", "-"),
      livemd: markdown,
      title: notebook.name,
      description: Keyword.fetch!(props, :description),
      image_url: Keyword.fetch!(props, :image_url),
      images: images
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

  defnotebook(:distributed_portals_with_elixir,
    description:
      "A fast-paced introduction to the Elixir language by building distributed data-transfer portals.",
    image_url: "/images/elixir-portal.jpeg",
    image_names: ["portal-drop.jpeg", "portal-list.jpeg"]
  )

  defnotebook(:elixir_and_livebook,
    description: "Learn how to use some of Elixir and Livebook's unique features together.",
    image_url: "/images/elixir.png"
  )

  defnotebook(:intro_to_nx,
    description:
      "Enter Numerical Elixir, experience the power of multi-dimensional arrays of numbers.",
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

  defnotebook(:vm_introspection,
    description: "Extract and visualize information about a remote running node.",
    image_url: "/images/vm_introspection.png"
  )

  defnotebook(:intro_to_kino,
    description: "Display and control rich and interactive widgets in Livebook.",
    image_url: "/images/kino.png"
  )

  @type notebook_info :: %{
          slug: String.t(),
          livemd: String.t(),
          title: String.t(),
          description: String.t(),
          image_url: String.t(),
          images: images()
        }

  @type images :: %{String.t() => binary()}

  @doc """
  Returns a list of example notebooks with metadata.
  """
  @spec notebook_infos() :: list(notebook_info())
  def notebook_infos() do
    [
      @intro_to_livebook,
      @distributed_portals_with_elixir,
      @elixir_and_livebook,
      @intro_to_vega_lite,
      @intro_to_kino,
      @intro_to_nx,
      @vm_introspection#, @intro_to_axon
    ]
  end

  @doc """
  Finds explore notebook by slug and returns the parsed data structure.

  Returns the notebook along with the images it uses as preloaded binaries.
  """
  @spec notebook_by_slug!(String.t()) :: {Livebook.Notebook.t(), images()}
  def notebook_by_slug!(slug) do
    notebook_infos()
    |> Enum.find(&(&1.slug == slug))
    |> case do
      nil ->
        raise NotFoundError, slug: slug

      notebook_info ->
        {notebook, []} = Livebook.LiveMarkdown.Import.notebook_from_markdown(notebook_info.livemd)
        {notebook, notebook_info.images}
    end
  end
end

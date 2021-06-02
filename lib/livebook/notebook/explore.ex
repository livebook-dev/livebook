defmodule Livebook.Notebook.Explore.Utils do
  @moduledoc false

  @doc """
  Defines a module attribute `attr` with notebook
  parsed from the corresponding file.
  """
  defmacro defnotebook(attr) do
    quote bind_quoted: [attr: attr] do
      filename = to_string(attr) <> ".livemd"
      path = Path.join([__DIR__, "explore", filename])

      notebook = Livebook.Notebook.Explore.Utils.notebook_from_file!(path)
      Module.put_attribute(__MODULE__, attr, notebook)

      @external_resource path
    end
  end

  def notebook_from_file!(path) do
    markdown = File.read!(path)
    {notebook, []} = Livebook.LiveMarkdown.Import.notebook_from_markdown(markdown)
    notebook
  end
end

defmodule Livebook.Notebook.Explore do
  @moduledoc false

  import Livebook.Notebook.Explore.Utils

  defnotebook(:intro_to_livebook)
  defnotebook(:intro_to_elixir)
  defnotebook(:intro_to_nx)
  defnotebook(:intro_to_axon)
  defnotebook(:intro_to_vega_lite)

  @type notebook_info :: %{
          slug: String.t(),
          notebook: Livebook.Notebook.t(),
          description: String.t(),
          image_url: String.t()
        }

  @doc """
  Returns a list of example notebooks with metadata.
  """
  @spec notebook_infos() :: list(notebook_info())
  def notebook_infos() do
    [
      %{
        slug: "intro-to-livebook",
        notebook: @intro_to_livebook,
        description: "Get to know Livebook, see how it works and explore its features.",
        image_url: "/images/logo.png"
      },
      %{
        slug: "intro-to-elixir",
        notebook: @intro_to_elixir,
        description: "New to Elixir? Learn about the language and its core concepts.",
        image_url: "/images/elixir.png"
      },
      %{
        slug: "intro-to-nx",
        notebook: @intro_to_nx,
        description:
          "Enter numerical Elixir, experience the power of multi-dimensional arrays of numbers.",
        image_url: "/images/nx.png"
      },
      %{
        slug: "intro-to-axon",
        notebook: @intro_to_axon,
        description: "Build Neural Networks in Elixir using a high-level, composable API.",
        image_url: "/images/axon.png"
      },
      %{
        slug: "intro-to-vega-lite",
        notebook: @intro_to_vega_lite,
        description: "Learn how to quickly create numerous plots for your data.",
        image_url: "/images/vega_lite.png"
      }
    ]
  end
end

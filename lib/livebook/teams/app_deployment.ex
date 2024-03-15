defmodule Livebook.Teams.AppDeployment do
  use Ecto.Schema
  alias Livebook.FileSystem

  @file_extension ".zip"

  @type zip_file :: {filename :: String.t(), content :: String.t()}
  @type zip_files :: list(zip_file())

  @type t :: %__MODULE__{
          id: String.t() | nil,
          filename: String.t() | nil,
          slug: String.t() | nil,
          sha: String.t() | nil,
          title: String.t() | nil,
          deployment_group_id: String.t() | nil,
          file: {:url, String.t()} | {:content, binary()} | nil,
          deployed_by: String.t() | nil,
          deployed_at: NaiveDateTime.t() | nil
        }

  @primary_key {:id, :string, autogenerate: false}
  embedded_schema do
    field :filename, :string
    field :slug, :string
    field :sha, :string
    field :title, :string
    field :deployment_group_id, :string
    field :file, :string
    field :deployed_by, :string

    timestamps(updated_at: nil, inserted_at: :deployed_at)
  end

  @doc """
  Creates a new app deployment from notebook.
  """
  @spec new(Livebook.Notebook.t(), String.t(), Livebook.FileSystem.File.t()) ::
          {:ok, t()} | {:warning, list(String.t())} | {:error, FileSystem.error()}
  def new(notebook, filename, files_dir) do
    filename = ensure_notebook_extension(filename)

    with {:ok, source} <- fetch_notebook_source(notebook),
         {:ok, files} <- build_and_check_file_entries(notebook, {filename, source}, files_dir),
         {:ok, {_, zip_content}} <- :zip.create(~c"app_deployment.zip", files, [:memory]) do
      md5_hash = :crypto.hash(:md5, zip_content)
      shasum = Base.encode16(md5_hash, case: :lower)

      {:ok,
       %__MODULE__{
         filename: shasum <> @file_extension,
         slug: notebook.app_settings.slug,
         sha: shasum,
         title: notebook.name,
         deployment_group_id: notebook.deployment_group_id,
         file: {:content, zip_content}
       }}
    end
  end

  defp ensure_notebook_extension(filename) do
    if String.ends_with?(filename, Livebook.LiveMarkdown.extension()) do
      to_charlist(filename)
    else
      to_charlist(filename <> Livebook.LiveMarkdown.extension())
    end
  end

  defp fetch_notebook_source(notebook) do
    case Livebook.LiveMarkdown.notebook_to_livemd(notebook) do
      {source, []} -> {:ok, source}
      {_, warnings} -> {:warning, warnings}
    end
  end

  defp build_and_check_file_entries(notebook, notebook_file_entry, files_dir) do
    notebook.file_entries
    |> Enum.filter(&(&1.type == :attachment))
    |> Enum.reduce_while({:ok, [notebook_file_entry]}, fn %{name: name}, {:ok, acc} ->
      file = Livebook.FileSystem.File.resolve(files_dir, name)

      with {:ok, true} <- Livebook.FileSystem.File.exists?(file),
           {:ok, content} <- Livebook.FileSystem.File.read(file) do
        {:cont, {:ok, [{to_charlist("files/" <> name), content} | acc]}}
      else
        {:ok, false} -> {:halt, {:error, "files/#{name}: doesn't exist"}}
        {:error, reason} -> {:halt, {:error, "files/#{name}: #{reason}"}}
      end
    end)
  end
end

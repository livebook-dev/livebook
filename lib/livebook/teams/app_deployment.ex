defmodule Livebook.Teams.AppDeployment do
  use Ecto.Schema
  alias Livebook.FileSystem

  @max_size 20 * 1024 * 1024

  @type t :: %__MODULE__{
          id: String.t() | nil,
          slug: String.t() | nil,
          version: String.t() | nil,
          sha: String.t() | nil,
          title: String.t() | nil,
          multi_session: boolean(),
          access_type: Livebook.Notebook.AppSettings.access_type(),
          hub_id: String.t() | nil,
          deployment_group_id: String.t() | nil,
          file: binary() | nil,
          deployed_by: String.t() | nil,
          deployed_at: DateTime.t() | nil
        }

  @access_types Livebook.Notebook.AppSettings.access_types()

  @primary_key {:id, :string, autogenerate: false}
  embedded_schema do
    field :slug, :string
    field :version, :string
    field :sha, :string
    field :title, :string
    field :multi_session, :boolean
    field :access_type, Ecto.Enum, values: @access_types
    field :hub_id, :string
    field :deployment_group_id, :string
    field :file, :string
    field :deployed_by, :string
    field :deployed_at, :utc_datetime
  end

  @doc """
  Creates a new app deployment from notebook.
  """
  @spec new(Livebook.Notebook.t(), Livebook.FileSystem.File.t()) ::
          {:ok, t()} | {:warning, list(String.t())} | {:error, FileSystem.error()}
  def new(notebook, files_dir) do
    with {:ok, source} <- fetch_notebook_source(notebook),
         {:ok, files} <- build_and_check_file_entries(notebook, source, files_dir),
         {:ok, {_, zip_content}} <- :zip.create(~c"app_deployment.zip", files, [:memory]),
         :ok <- validate_size(zip_content) do
      md5_hash = :crypto.hash(:md5, zip_content)
      shasum = Base.encode16(md5_hash, case: :lower)

      {:ok,
       %__MODULE__{
         slug: notebook.app_settings.slug,
         sha: shasum,
         title: notebook.name,
         multi_session: notebook.app_settings.multi_session,
         access_type: notebook.app_settings.access_type,
         hub_id: notebook.hub_id,
         deployment_group_id: notebook.deployment_group_id,
         file: zip_content
       }}
    end
  end

  defp fetch_notebook_source(notebook) do
    case Livebook.LiveMarkdown.notebook_to_livemd(notebook) do
      {source, []} -> {:ok, source}
      {_, warnings} -> {:warning, warnings}
    end
  end

  defp build_and_check_file_entries(notebook, source, files_dir) do
    notebook.file_entries
    |> Enum.filter(&(&1.type == :attachment))
    |> Enum.reduce_while({:ok, [{~c"notebook.livemd", source}]}, fn %{name: name}, {:ok, acc} ->
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

  defp validate_size(data) do
    if byte_size(data) <= @max_size do
      :ok
    else
      {:error, "the notebook and its attachments have exceeded the maximum size of 20MB"}
    end
  end
end

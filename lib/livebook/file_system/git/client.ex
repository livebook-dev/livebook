defmodule Livebook.FileSystem.Git.Client do
  alias Livebook.FileSystem

  @doc """
  Sends a request to the repository to get list of files.
  """
  @spec list_files(FileSystem.Git.t(), String.t()) ::
          {:ok, list(String.t())} | {:error, FileSystem.error()}
  def list_files(file_system, path)

  def list_files(%FileSystem.Git{} = file_system, "/") do
    git_dir = git_dir(file_system)
    ls_tree(git_dir, "HEAD")
  end

  def list_files(%FileSystem.Git{} = file_system, path) do
    git_dir = git_dir(file_system)
    ls_tree(git_dir, "HEAD", [path])
  end

  @doc """
  Sends a request to the repository to read a file.
  """
  @spec read_file(FileSystem.Git.t(), String.t()) ::
          {:ok, String.t()} | {:error, FileSystem.error()}
  def read_file(%FileSystem.Git{} = file_system, path) do
    git_dir = git_dir(file_system)
    show(git_dir, "HEAD", path)
  end

  @doc """
  Sends a request to the repository to read file ETag.
  """
  @spec etag(FileSystem.Git.t(), String.t()) :: {:ok, String.t()} | {:error, FileSystem.error()}
  def etag(%FileSystem.Git{} = file_system, path) do
    git_dir = git_dir(file_system)
    rev_parse(git_dir, "HEAD", path)
  end

  defp ls_tree(git_dir, branch, args \\ []) do
    with {:ok, result} <- git(git_dir, ["ls-tree", "--name-only", branch] ++ args) do
      {:ok, String.split(result, "\n", trim: true)}
    end
  end

  defp show(git_dir, branch, path) do
    git(git_dir, ["show", "#{branch}:#{path}"])
  end

  defp rev_parse(git_dir, branch, path) do
    with {:ok, etag} <- git(git_dir, ["rev-parse", "#{branch}:#{path}"]) do
      {:ok, String.trim(etag)}
    end
  end

  defp git(git_dir, args) do
    if git = System.find_executable("git") do
      case System.cmd(git, args, cmd_opts(git_dir)) do
        {result, 0} -> {:ok, result}
        {error, _} -> {:error, String.trim(error)}
      end
    else
      {:error, "'git' executable not found"}
    end
  end

  @cmd_opts [use_stdio: true, stderr_to_stdout: true]

  defp cmd_opts(git_dir) do
    if File.exists?(git_dir) do
      Keyword.merge(@cmd_opts, cd: git_dir)
    else
      @cmd_opts
    end
  end

  defp git_dir(file_system) do
    git_dir = Path.join(System.tmp_dir!(), file_system.id)

    args = [
      "clone",
      "--bare",
      "--depth=1",
      "--single-branch",
      file_system.repo_url,
      git_dir
    ]

    if File.exists?(git_dir) do
      git_dir
    else
      case git(git_dir, args) do
        {:ok, _} -> git_dir
        {:error, reason} -> raise reason
      end
    end
  end
end

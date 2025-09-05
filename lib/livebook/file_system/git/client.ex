defmodule Livebook.FileSystem.Git.Client do
  alias Livebook.FileSystem

  @doc """
  Returns a list of files from given repository and given path.
  """
  @spec list_files(FileSystem.Git.t(), String.t()) ::
          {:ok, list(String.t())} | {:error, FileSystem.error()}
  def list_files(%FileSystem.Git{} = file_system, path) do
    path = relative_path(path)

    with {:ok, git_dir} <- fetch_repository(file_system) do
      ls_tree(git_dir, file_system.branch, path)
    end
  end

  @doc """
  Returns the content of the given file from given repository.
  """
  @spec read_file(FileSystem.Git.t(), String.t()) ::
          {:ok, String.t()} | {:error, FileSystem.error()}
  def read_file(%FileSystem.Git{} = file_system, path) do
    path = relative_path(path)

    with {:ok, git_dir} <- fetch_repository(file_system) do
      show(git_dir, file_system.branch, path)
    end
  end

  @doc """
  Returns the ETag of the given file from given repository.
  """
  @spec etag(FileSystem.Git.t(), String.t()) :: {:ok, String.t()} | {:error, FileSystem.error()}
  def etag(%FileSystem.Git{} = file_system, path) do
    path = relative_path(path)

    with {:ok, git_dir} <- fetch_repository(file_system) do
      rev_parse(git_dir, file_system.branch, path)
    end
  end

  @doc """
  Returns if the given path exists in given repository.
  """
  @spec exists?(FileSystem.Git.t(), String.t()) :: {:ok, boolean()} | {:error, FileSystem.error()}
  def exists?(%FileSystem.Git{} = file_system, path) do
    with {:ok, keys} <- list_files(file_system, path) do
      {:ok, keys != []}
    end
  end

  @doc """
  Fetches the repository with latest changes and checkout branch.
  """
  @spec fetch(FileSystem.Git.t()) :: :ok | {:error, FileSystem.error()}
  def fetch(%FileSystem.Git{} = file_system) do
    with {:ok, git_dir} <- fetch_repository(file_system) do
      with_ssh_key_file(file_system, fn key_path ->
        fetch(git_dir, file_system.branch, key_path)
      end)
    end
  end

  @doc """
  Removes the repository locally.
  """
  @spec remove_repository(FileSystem.Git.t()) :: :ok | {:error, FileSystem.error()}
  def remove_repository(%FileSystem.Git{} = file_system) do
    git_dir = FileSystem.Git.git_dir(file_system)
    key_path = FileSystem.Git.key_path(file_system)

    with {:ok, _} <- File.rm_rf(git_dir),
         :ok <- File.rm(key_path) do
      :ok
    else
      {:error, reason, _file} -> FileSystem.Utils.posix_error(reason)
      error -> error
    end
  end

  @ls_tree_format "--format=%(objecttype) %(path)"

  defp ls_tree(git_dir, branch, path) do
    with {:ok, result} <- git(git_dir, ["ls-tree", @ls_tree_format, branch, path]) do
      {:ok,
       result
       |> String.split("\n", trim: true)
       |> Enum.map(&normalize_dir_path/1)}
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

  defp clone(git_dir, repo_url, key_path) do
    with {:ok, ssh} <- fetch_executable("ssh") do
      git(git_dir, ["clone", "--bare", "--depth=1", repo_url, git_dir], env_opts(ssh, key_path))
    end
  end

  defp fetch(git_dir, branch, key_path) do
    with {:ok, ssh} <- fetch_executable("ssh"),
         {:ok, _} <-
           git(git_dir, ["fetch", "origin", "#{branch}:#{branch}"], env_opts(ssh, key_path)) do
      :ok
    end
  end

  defp git(git_dir, args, opts \\ []) do
    with {:ok, git} <- fetch_executable("git") do
      case System.cmd(git, args, cmd_opts(git_dir, opts)) do
        {result, 0} -> {:ok, result}
        {error, _} -> {:error, String.trim(error)}
      end
    end
  end

  @cmd_opts [use_stdio: true, stderr_to_stdout: true]

  defp cmd_opts(git_dir, opts) do
    opts = Keyword.merge(@cmd_opts, opts)

    if File.exists?(git_dir) do
      Keyword.merge(opts, cd: git_dir)
    else
      opts
    end
  end

  defp env_opts(ssh, key_path) do
    [env: %{"GIT_SSH_COMMAND" => "#{ssh} -i #{key_path}"}]
  end

  defp fetch_repository(file_system) do
    git_dir = FileSystem.Git.git_dir(file_system)

    if File.exists?(git_dir) do
      {:ok, git_dir}
    else
      with_ssh_key_file(file_system, fn key_path ->
        with {:ok, _} <- clone(git_dir, file_system.repo_url, key_path) do
          {:ok, git_dir}
        end
      end)
    end
  end

  @begin_key "-----BEGIN OPENSSH PRIVATE KEY-----"
  @end_key "-----END OPENSSH PRIVATE KEY-----"

  defp with_ssh_key_file(file_system, fun) when is_function(fun, 1) do
    File.mkdir_p!(FileSystem.Git.ssh_path())
    key_path = FileSystem.Git.key_path(file_system)

    if not File.exists?(key_path) do
      File.write!(key_path, """
      #{@begin_key}
      #{normalize_ssh_key(file_system.key)}
      #{@end_key}
      """)

      File.chmod!(key_path, 0o600)
    end

    result = fun.(key_path)

    if File.exists?(key_path) do
      File.rm!(key_path)
    end

    result
  end

  defp normalize_ssh_key(key) do
    key
    |> String.replace_prefix(@begin_key, "")
    |> String.replace_suffix(@end_key, "")
    |> String.trim()
    |> String.split("\s")
    |> Enum.join("\n")
  end

  defp normalize_dir_path(<<"blob ", path::binary>>), do: normalize_path(path)
  defp normalize_dir_path(<<"tree ", path::binary>>), do: normalize_path(path <> "/")

  defp normalize_path("/"), do: "."
  defp normalize_path("/" <> _ = path), do: path
  defp normalize_path(path), do: "/" <> path

  defp relative_path("/"), do: "."
  defp relative_path("/" <> path), do: path
  defp relative_path(path), do: path

  defp fetch_executable(program) do
    if bin = System.find_executable(program) do
      {:ok, bin}
    else
      {:error, "'#{program}' executable not found"}
    end
  end
end

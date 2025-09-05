defmodule Livebook.FileSystem.Git.Client do
  alias Livebook.FileSystem

  @doc """
  Sends a request to the repository to get list of files.
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
  Sends a request to the repository to read a file.
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
  Sends a request to the repository to read file ETag.
  """
  @spec etag(FileSystem.Git.t(), String.t()) :: {:ok, String.t()} | {:error, FileSystem.error()}
  def etag(%FileSystem.Git{} = file_system, path) do
    path = relative_path(path)

    with {:ok, git_dir} <- fetch_repository(file_system) do
      rev_parse(git_dir, file_system.branch, path)
    end
  end

  @doc """
  Sends a request to the repository to check if given file exists.
  """
  @spec exists?(FileSystem.Git.t(), String.t()) :: {:ok, boolean()} | {:error, FileSystem.error()}
  def exists?(%FileSystem.Git{} = file_system, path) do
    with {:ok, keys} <- list_files(file_system, path) do
      {:ok, keys != []}
    end
  end

  defp ls_tree(git_dir, branch, path) do
    with {:ok, result} <-
           git(git_dir, ["ls-tree", "--format=%(objecttype) %(path)", branch, path]) do
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
      git(git_dir, ["clone", "--bare", "--depth=1", repo_url, git_dir],
        env: %{"GIT_SSH_COMMAND" => "#{ssh} -i #{key_path}"}
      )
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

  defp fetch_repository(file_system) do
    git_dir = Path.join(System.tmp_dir!(), file_system.id)

    if File.exists?(git_dir) do
      {:ok, git_dir}
    else
      key_path = persist_ssh_key_file(file_system)
      result = clone(git_dir, file_system.repo_url, key_path)
      File.rm!(key_path)

      with {:ok, _} <- result, do: {:ok, git_dir}
    end
  end

  @begin_key "-----BEGIN OPENSSH PRIVATE KEY-----"
  @end_key "-----END OPENSSH PRIVATE KEY-----"

  defp persist_ssh_key_file(file_system) do
    key_path = Path.join([System.user_home!(), ".ssh", "id_" <> file_system.id])

    if not File.exists?(key_path) do
      File.write!(key_path, """
      #{@begin_key}
      #{normalize_ssh_key(file_system.key)}
      #{@end_key}
      """)

      # chmod 600
      File.chmod!(key_path, 384)
    end

    key_path
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

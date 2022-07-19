defmodule AppBundler.Utils do
  @moduledoc false

  require Logger

  def cmd!(bin, args, opts \\ []) do
    opts = Keyword.put_new(opts, :into, IO.stream())
    {_, status} = System.cmd(bin, args, opts)

    if status != 0 do
      raise "command exited with #{status}"
    end
  end

  def shell!(command, opts \\ []) do
    opts = Keyword.put_new(opts, :into, IO.stream())
    {_, 0} = System.shell(command, opts)
  end

  def ensure_executable(url) do
    ensure_executable(url, :no_verify)
  end

  def ensure_executable(url, expected_sha256) do
    tmp_dir = Path.join(System.tmp_dir!(), Path.basename(url, Path.extname(url)))
    path = Path.join(tmp_dir, Path.basename(url))

    if File.exists?(path) do
      verify(File.read!(path), expected_sha256)
    else
      File.mkdir_p!(tmp_dir)
      body = download_and_verify(url, expected_sha256)
      File.write!(path, body)

      if Path.extname(path) == ".zip" do
        {:ok, _} = :zip.extract(String.to_charlist(path), cwd: tmp_dir)
      else
        File.chmod!(path, 0o600)
      end
    end

    path
  end

  def ensure_executable(url, expected_sha256, basename) do
    path = ensure_executable(url, expected_sha256)
    Path.join(Path.dirname(path), basename)
  end

  defp download_and_verify(url, expected_sha256) do
    url
    |> download_unverified()
    |> verify(expected_sha256)
  end

  defp download_unverified(url) do
    Logger.debug("downloading #{url}")
    http_options = [ssl: [verify: :verify_none]]
    options = []
    headers = []
    request = {url, headers}
    {:ok, {{_, 200, _}, _, body}} = :httpc.request(:get, request, http_options, options)
    body
  end

  defp verify(data, :no_verify) do
    data
  end

  defp verify(data, expected_sha256) do
    actual_sha256 = :crypto.hash(:sha256, data) |> Base.encode16(case: :lower)

    if expected_sha256 == actual_sha256 do
      data
    else
      raise """
      sha256 mismatch
      expected: #{expected_sha256}
      got:      #{actual_sha256}\
      """
    end
  end

  def normalize_icon_path(path) when is_binary(path) do
    path
  end

  def normalize_icon_path(path_per_os) when is_list(path_per_os) do
    Keyword.fetch!(path_per_os, AppBundler.os())
  end

  def copy_dir(from, to, options \\ []) do
    File.mkdir_p!(Path.dirname(to))
    log(:green, "creating", Path.relative_to_cwd(to), options)
    File.cp_r!(from, to)
  end

  def copy_file(source, target, options \\ []) do
    create_file(target, File.read!(source), options)
  end

  def copy_template(source, target, assigns, options \\ []) do
    create_file(target, EEx.eval_file(source, assigns: assigns, trim: true), options)
  end

  def create_file(path, contents, options \\ []) when is_binary(path) do
    log(:green, :creating, Path.relative_to_cwd(path), options)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, contents)
  end

  def log(color, command, message, options \\ []) do
    unless options[:quiet] do
      Mix.shell().info([color, "* #{command} ", :reset, message])
    end
  end
end

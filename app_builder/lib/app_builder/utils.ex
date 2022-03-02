defmodule AppBuilder.Utils do
  @moduledoc false

  require Logger

  def cmd!(bin, args, opts \\ []) do
    opts = Keyword.put_new(opts, :into, IO.stream())
    {_, 0} = System.cmd(bin, args, opts)
  end

  def shell!(command, opts \\ []) do
    opts = Keyword.put_new(opts, :into, IO.stream())
    {_, 0} = System.shell(command, opts)
  end

  def ensure_executable(url, expected_sha256) do
    tmp_dir = Path.join(System.tmp_dir!(), Path.basename(url, Path.extname(url)))
    path = Path.join(tmp_dir, Path.basename(url))

    unless File.exists?(path) do
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
end

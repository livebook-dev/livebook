defmodule Standalone do
  @moduledoc false
  require Logger

  @doc """
  Copies OTP into the release.
  """
  @spec copy_otp(Mix.Release.t()) :: Mix.Release.t()
  def copy_otp(release) do
    vendor_otp_path = "#{release.path}/vendor/otp"
    File.mkdir_p!(Path.dirname(vendor_otp_path))
    File.rm_rf!(vendor_otp_path)
    File.cp_r!(:code.root_dir(), vendor_otp_path)

    File.cd!(vendor_otp_path, fn ->
      leftovers = File.ls!(".") -- ["COPYRIGHT", "usr", "bin", "lib", "erts-#{release.erts_version}"]
      Enum.each(leftovers, &File.rm_rf!/1)
    end)

    release
  end

  @doc """
  Copies Elixir into the release.
  """
  @spec copy_elixir(Mix.Release.t(), elixir_version :: String.t()) :: Mix.Release.t()
  def copy_elixir(release, elixir_version) do
    standalone_destination = Path.join(release.path, "vendor/elixir")
    download_elixir_at_destination(standalone_destination, elixir_version)

    filenames =
      case AppBundler.os() do
        :macos ->
          ["elixir", "elixirc", "mix", "iex"]

        :windows ->
          ["elixir.bat", "elixirc.bat", "mix.bat", "iex.bat"]
      end

    Enum.map(filenames, &make_executable(Path.join(standalone_destination, "bin/#{&1}")))

    release
  end

  defp download_elixir_at_destination(destination, version) do
    url = "https://repo.hex.pm/builds/elixir/v#{version}-otp-#{System.otp_release()}.zip"
    path = Path.join(System.tmp_dir!(), "elixir_#{version}.zip")

    unless File.exists?(path) do
      binary = fetch_body!(url)
      File.write!(path, binary, [:binary])
    end

    :zip.unzip(String.to_charlist(path), cwd: destination)
  end

  @doc """
  Copies Hex into the release.
  """
  @spec copy_hex(Mix.Release.t()) :: Mix.Release.t()
  def copy_hex(release) do
    release_archives_dir = Path.join(release.path, "vendor/archives")
    File.mkdir_p!(release_archives_dir)

    hex_version = Keyword.fetch!(Application.spec(:hex), :vsn)
    source_hex_path = Path.join(Mix.path_for(:archives), "hex-#{hex_version}")
    release_hex_path = Path.join(release_archives_dir, "hex-#{hex_version}")
    cp_r!(source_hex_path, release_hex_path)

    release
  end

  @doc """
  Copies Rebar3 into the release.
  """
  @spec copy_rebar3(Mix.Release.t(), version :: String.t()) :: Mix.Release.t()
  def copy_rebar3(release, version) do
    url = "https://github.com/erlang/rebar3/releases/download/#{version}/rebar3"
    path = Path.join(System.tmp_dir!(), "rebar3_#{version}")

    unless File.exists?(path) do
      binary = fetch_body!(url)
      File.write!(path, binary, [:binary])
    end

    destination = Path.join(release.path, "vendor/rebar3")
    File.cp!(path, destination)
    make_executable(destination)

    release
  end

  defp fetch_body!(url) do
    Logger.debug("Downloading #{url}")

    case Livebook.Utils.HTTP.request(:get, url, timeout: :infinity) do
      {:ok, 200, _headers, body} ->
        body

      {:error, error} ->
        raise "couldn't fetch #{url}: #{inspect(error)}"
    end
  end

  defp make_executable(path), do: File.chmod!(path, 0o755)

  defp cp_r!(source, destination) do
    File.cp_r!(source, destination, fn _, _ -> false end)
  end
end

defmodule Standalone do
  @moduledoc false
  require Logger

  @doc """
  Copies OTP into the release.
  """
  @spec copy_otp(Mix.Release.t()) :: Mix.Release.t()
  def copy_otp(release) do
    erts_source = Path.join(:code.root_dir(), "erts-#{release.erts_version}")
    otp_bin_dir = Path.join(:code.root_dir(), "bin")
    otp_lib_dir = :code.lib_dir()

    # 1. copy erts/{bin,include}
    release_erts_bin_dir = Path.join(release.path, "erts-#{release.erts_version}/bin")
    File.mkdir_p!(release_erts_bin_dir)

    cp_r!(Path.join(erts_source, "bin"), release_erts_bin_dir)

    File.rm(Path.join(release_erts_bin_dir, "erl"))
    File.rm(Path.join(release_erts_bin_dir, "erl.ini"))

    File.write!(Path.join(release_erts_bin_dir, "erl"), ~S"""
    #!/bin/sh
    SELF=$(readlink "$0" || true)
    if [ -z "$SELF" ]; then SELF="$0"; fi
    BINDIR="$(cd "$(dirname "$SELF")" && pwd -P)"
    ROOTDIR="${ERL_ROOTDIR:-"$(dirname "$(dirname "$BINDIR")")"}"
    EMU=beam
    PROGNAME=$(echo "$0" | sed 's/.*\///')
    export EMU
    export ROOTDIR
    export BINDIR
    export PROGNAME
    exec "$BINDIR/erlexec" ${1+"$@"}
    """)

    make_executable(Path.join(release_erts_bin_dir, "erl"))

    release_erts_include_dir = Path.join(release.path, "erts-#{release.erts_version}/include")
    cp_r!(Path.join(erts_source, "include"), release_erts_include_dir)

    # 2. copy lib
    release_lib_dir = Path.join(release.path, "lib")
    cp_r!(otp_lib_dir, release_lib_dir)

    for dir <- Path.wildcard("#{release_lib_dir}/*/doc/{xml,html,pdf}") do
      File.rm_rf!(dir)
    end

    # 3. copy boot files
    release_bin_dir = Path.join(release.path, "bin")

    for file <- Path.wildcard(Path.join(otp_bin_dir, "*.boot")) do
      File.cp!(file, Path.join(release_bin_dir, Path.basename(file)))
    end

    %{release | erts_source: erts_source}
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
    Logger.debug("Downloading Elixir from #{url}")

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

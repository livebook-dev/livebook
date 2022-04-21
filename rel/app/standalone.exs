defmodule Standalone do
  @moduledoc false
  require Logger

  @doc """
  Copies OTP into the release.
  """
  @spec copy_otp(Mix.Release.t(), otp_version :: String.t()) :: Mix.Release.t()
  def copy_otp(release, otp_version) do
    if Mix.env() != :dev do
      ensure_otp_version(otp_version)
    end

    {erts_source, otp_bin_dir, otp_lib_dir} = otp_dirs()

    # 1. copy erts/bin
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

    # 2. copy lib
    release_lib_dir = Path.join(release.path, "lib")
    cp_r!(otp_lib_dir, release_lib_dir)

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
      case os() do
        :macos ->
          ["elixir", "elixirc", "mix", "iex"]

        :windows ->
          ["elixir.bat", "elixirc.bat", "mix.bat", "iex.bat"]
      end

    Enum.map(filenames, &make_executable(Path.join(standalone_destination, "bin/#{&1}")))

    release
  end

  defp download_elixir_at_destination(destination, version) do
    url = "https://github.com/elixir-lang/elixir/releases/download/v#{version}/Precompiled.zip"
    path = Path.join(System.tmp_dir!(), "elixir_#{version}.zip")

    unless File.exists?(path) do
      binary = fetch_body!(url)
      File.write!(path, binary, [:binary])
    end

    :zip.unzip(String.to_charlist(path), cwd: destination)
  end

  defp otp_dirs do
    version = :erlang.system_info(:version)
    root_dir = :code.root_dir()

    {:filename.join(root_dir, 'erts-#{version}'), :filename.join(root_dir, 'bin'),
     :code.lib_dir()}
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

  defp os() do
    case :os.type() do
      {:unix, :darwin} -> :macos
      {:win32, _} -> :windows
    end
  end

  defp cp_r!(source, destination) do
    File.cp_r!(source, destination, fn _, _ -> false end)
  end

  defp ensure_otp_version(expected_otp_version) do
    actual_otp_version = otp_version()

    if actual_otp_version != expected_otp_version do
      raise "expected OTP #{expected_otp_version}, got: #{actual_otp_version}"
    end
  end

  # From https://github.com/fishcakez/dialyze/blob/6698ae582c77940ee10b4babe4adeff22f1b7779/lib/mix/tasks/dialyze.ex#L168
  defp otp_version do
    major = :erlang.system_info(:otp_release) |> List.to_string()
    vsn_file = Path.join([:code.root_dir(), "releases", major, "OTP_VERSION"])

    try do
      vsn_file |> File.read!() |> String.split("\n", trim: true)
    else
      [full] -> full
      _ -> major
    catch
      :error, _ -> major
    end
  end
end

defmodule Standalone do
  @moduledoc false
  require Logger

  @doc """
  Copies OTP into the release.
  """
  @spec copy_otp(Mix.Release.t(), otp_version :: String.t()) :: Mix.Release.t()
  def copy_otp(release, otp_version) do
    expected_otp_version = otp_version()

    if otp_version != expected_otp_version do
      raise "expected OTP #{expected_otp_version}, got: #{otp_version}"
    end

    {erts_source, erts_bin_dir, erts_lib_dir, _erts_version} = erts_data()

    erts_destination_source = Path.join(release.path, "vendor/bin")
    File.mkdir_p!(erts_destination_source)

    erts_source
    |> Path.join("bin")
    |> File.cp_r!(erts_destination_source, fn _, _ -> false end)

    _ = File.rm(Path.join(erts_destination_source, "erl"))
    _ = File.rm(Path.join(erts_destination_source, "erl.ini"))

    if os() == :macos do
      erts_destination_source
      |> Path.join("erl")
      |> File.write!(~S"""
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

      executable!(Path.join(erts_destination_source, "erl"))
    end

    # Copy lib
    erts_destination_lib = Path.join(release.path, "lib")
    File.mkdir_p!(erts_destination_lib)

    erts_lib_dir
    |> File.cp_r!(erts_destination_lib, fn _, _ -> false end)

    # copy *.boot files to <resource_path>/bin
    erts_destination_bin = Path.join(release.path, "bin")

    boot_files =
      erts_bin_dir
      |> Path.join("*.boot")
      |> Path.wildcard()
      |> Enum.map(&(String.split(&1, "/") |> List.last()))

    File.mkdir_p!(erts_destination_bin)

    for boot_file <- boot_files do
      erts_bin_dir |> Path.join(boot_file) |> File.cp!(Path.join(erts_destination_bin, boot_file))
    end

    %{release | erts_source: erts_source}
  end

  # From https://github.com/fishcakez/dialyze/blob/6698ae582c77940ee10b4babe4adeff22f1b7779/lib/mix/tasks/dialyze.ex#L168
  defp otp_version do
    major = :erlang.system_info(:otp_release) |> List.to_string()
    vsn_file = Path.join([:code.root_dir(), "releases", major, "OTP_VERSION"])

    try do
      {:ok, contents} = File.read(vsn_file)
      String.split(contents, "\n", trim: true)
    else
      [full] -> full
      _ -> major
    catch
      :error, _ -> major
    end
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

    Enum.map(filenames, &executable!(Path.join(standalone_destination, "bin/#{&1}")))

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

  defp erts_data do
    version = :erlang.system_info(:version)

    {:filename.join(:code.root_dir(), 'erts-#{version}'), :filename.join(:code.root_dir(), 'bin'),
     :code.lib_dir(), version}
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

  defp executable!(path), do: File.chmod!(path, 0o755)

  defp os() do
    case :os.type() do
      {:unix, :darwin} -> :macos
      {:win32, _} -> :windows
    end
  end
end

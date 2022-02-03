defmodule Standalone do
  @moduledoc false
  require Logger

  @doc """
  Copies ERTS into the release.
  """
  @spec copy_erlang(Mix.Release.t()) :: Mix.Release.t()
  def copy_erlang(release) do
    {erts_source, erts_bin_dir, erts_lib_dir, _erts_version} = erts_data()

    erts_destination_source = Path.join(release.path, "vendor/bin")
    File.mkdir_p!(erts_destination_source)

    erts_source
    |> Path.join("bin")
    |> File.cp_r!(erts_destination_source, fn _, _ -> false end)

    _ = File.rm(Path.join(erts_destination_source, "erl"))
    _ = File.rm(Path.join(erts_destination_source, "erl.ini"))

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

    # Copy lib
    erts_destination_lib = Path.join(release.path, "lib")
    File.mkdir_p!(erts_destination_lib)

    erts_lib_dir
    |> File.cp_r!(erts_destination_lib, fn _, _ -> false end)

    # copy *.boot files to <resource_path>/bin
    erts_destination_bin =  Path.join(release.path, "bin")

    boot_files = erts_bin_dir |> Path.join("*.boot") |> Path.wildcard() |> Enum.map(& String.split(&1, "/") |> List.last())

    File.mkdir_p!(erts_destination_bin)

    for boot_file <- boot_files do
      erts_bin_dir |> Path.join(boot_file) |> File.cp!(Path.join(erts_destination_bin, boot_file))
    end

    %{release | erts_source: erts_source}
  end

  @doc """
  Copies elixir into the release.
  """
  @spec copy_elixir(Mix.Release.t(), elixir_version :: String.t()) :: Mix.Release.t()
  def copy_elixir(release, elixir_version) do
    # download and unzip
    standalone_destination = Path.join(release.path, "vendor/elixir")
    download_elixir_at_destination(standalone_destination, elixir_version)

    # make executable
    ["elixir", "elixirc", "mix", "iex"]
    |> Enum.map(&(executable!(Path.join(standalone_destination, "bin/#{&1}"))))

    release
  end

  defp download_elixir_at_destination(destination, elixir_version) do
    url = "https://github.com/elixir-lang/elixir/releases/download/v#{elixir_version}/Precompiled.zip"
    binary = fetch_body!(url)
    File.write!("/tmp/elixir_#{elixir_version}.zip", binary, [:binary])
    :zip.unzip('/tmp/elixir_#{elixir_version}.zip', cwd: destination)
  end

  defp erts_data do
    version = :erlang.system_info(:version)
    {:filename.join(:code.root_dir(), 'erts-#{version}'), :filename.join(:code.root_dir(), 'bin'), :code.lib_dir(), version}
  end

  defp fetch_body!(url) do
    Logger.debug("Downloading elixir from #{url}")
    case Livebook.Utils.HTTP.request(:get, url, [timeout: :infinity]) do
      {:ok, 200, _headers, body} ->
        body

      {:error, error}  ->
        raise "couldn't fetch #{url}: #{inspect(error)}"
    end
  end

  defp executable!(path), do: File.chmod!(path, 0o755)

end

defmodule AppBuilder do
  require Logger

  defdelegate build_mac_app(release, options), to: AppBuilder.MacOS

  defdelegate build_mac_app_dmg(release, options), to: AppBuilder.MacOS

  @doc """
  Copies ERTS into the release.
  """
  @spec copy_erlang(Mix.Release.t()) :: Mix.Release.t()
  def copy_erlang(release) do
    {erts_source, erts_bin_dir, erts_lib_dir, erts_version} = erts_data()

    erts_destination_source = Path.join(release.path, "erts-#{erts_version}/bin")
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

    # copy start.boot to <resource_path>/rel/bin
    erts_destination_bin =  Path.join(release.path, "/bin")
    start_boot_file = Path.join(erts_destination_bin, "start.boot")
    File.mkdir_p!(erts_destination_bin)

    erts_bin_dir
    |> Path.join("start.boot")
    |> File.cp!(start_boot_file, fn _, _ -> false end)

    %{release | erts_source: erts_source}
  end

  @erts_bin [~s[ERTS_BIN="$ERTS_BIN"], ~s[ERTS_BIN=!ERTS_BIN!]]

  defp replace_erts_bin(contents, release, new_path) do
    if release.erts_source do
      String.replace(contents, @erts_bin, ~s[ERTS_BIN=#{new_path}])
    else
      contents
    end
  end

  @doc """
  Copies elixir into the release.
  """
  @spec copy_elixir(Mix.Release.t()) :: Mix.Release.t()
  def copy_elixir(release) do
    include_executables_for = Keyword.get(release.options, :include_executables_for, [:unix])

    elixir_bin_path = Application.app_dir(:elixir, "../../bin")
    bin_path = Path.join(release.path, "bin")
    File.mkdir_p!(bin_path)

    # download and unzip
    standalone_destination = Path.join(release.path, "elixir")
    download_elixir_at_destination(standalone_destination)

    # patch elixir file to look for the right erts <resource_path>/rel/releases/#{release.version}/elixir
    patch_elixir(include_executables_for, release,
      fn filename ->
        Path.join(elixir_bin_path, filename)
      end,
      fn filename ->
        Path.join(release.version_path, filename)
      end
    )

    # patch elixir file to look for the right erts <resource_path>/rel/elixir/bin/elixir
    patch_elixir(include_executables_for, release,
      fn filename ->
        Path.join([standalone_destination, "bin", filename])
      end,
      fn filename ->
        Path.join([standalone_destination, "bin", filename])
      end
    )

    release
  end

  @elixir_standalone_version "1.13.2"

  defp download_elixir_at_destination(destination) do
    url = "https://github.com/elixir-lang/elixir/releases/download/v#{@elixir_standalone_version}/Precompiled.zip"
    binary = fetch_body!(url)
    File.write!("/tmp/elixir_#{@elixir_standalone_version}.zip", binary, [:binary])
    :zip.unzip('/tmp/elixir_#{@elixir_standalone_version}.zip', cwd: destination)
  end

  defp patch_elixir(include_executables_for, release, fn_source, fn_target) do
    for os <- include_executables_for do
      for {filename, contents_fun} <- elixir_cli_for(os, release) do
        source = fn_source.(filename)

        if File.regular?(source) do
          target = fn_target.(filename)

          File.write!(target, contents_fun.(source))
          executable!(target)

        else
          skipping("#{filename} for #{os} (bin/#{filename} not found in the Elixir installation)")
        end
      end
    end
  end

  defp elixir_cli_for(:unix, release) do
    [
      {"elixir",
       &(&1
         |> File.read!()
         |> String.replace(~s[ -pa "$SCRIPT_PATH"/../lib/*/ebin], "")
         |> replace_erts_bin(release, ~s["$SCRIPT_PATH"/../../erts-#{release.erts_version}/bin/]))},
      {"iex", &File.read!/1}
    ]
  end

  defp elixir_cli_for(:windows, release) do
    [
      {"elixir.bat",
       &(&1
         |> File.read!()
         |> String.replace(~s[goto expand_erl_libs], ~s[goto run])
         |> replace_erts_bin(release, ~s[%~dp0\\..\\..\\erts-#{release.erts_version}\\bin\\]))},
      {"iex.bat", &File.read!/1}
    ]
  end

  defp erts_data do
    version = :erlang.system_info(:version)
    {:filename.join(:code.root_dir(), 'erts-#{version}'), :filename.join(:code.root_dir(), 'bin'), :code.lib_dir(), version}
  end

  defp fetch_body!(url) do
    url = String.to_charlist(url)
    Logger.debug("Downloading elixir from #{url}")

    {:ok, _} = Application.ensure_all_started(:inets)
    {:ok, _} = Application.ensure_all_started(:ssl)

    if proxy = System.get_env("HTTP_PROXY") || System.get_env("http_proxy") do
      Logger.debug("Using HTTP_PROXY: #{proxy}")
      %{host: host, port: port} = URI.parse(proxy)
      :httpc.set_options([{:proxy, {{String.to_charlist(host), port}, []}}])
    end

    if proxy = System.get_env("HTTPS_PROXY") || System.get_env("https_proxy") do
      Logger.debug("Using HTTPS_PROXY: #{proxy}")
      %{host: host, port: port} = URI.parse(proxy)
      :httpc.set_options([{:https_proxy, {{String.to_charlist(host), port}, []}}])
    end

    # https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/inets
    cacertfile = CAStore.file_path() |> String.to_charlist()

    http_options = [
      ssl: [
        verify: :verify_peer,
        cacertfile: cacertfile,
        depth: 2,
        customize_hostname_check: [
          match_fun: :public_key.pkix_verify_hostname_match_fun(:https)
        ]
      ]
    ]

    options = [body_format: :binary]

    case :httpc.request(:get, {url, []}, http_options, options) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        body

      other ->
        raise "couldn't fetch #{url}: #{inspect(other)}"
    end
  end

  defp skipping(message) do
    Mix.shell().info([:yellow, "* skipping ", :reset, message])
  end

  defp executable!(path), do: File.chmod!(path, 0o755)
end

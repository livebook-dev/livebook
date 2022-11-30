defmodule ElixirKit.Bundler do
  @moduledoc false

  @openssl_version "1.1.1s"
  @otp_version "25.1.2"
  @elixirkit_dir Path.expand("../..", __DIR__)

  def bundle_release(release) do
    if otp_version() != @otp_version do
      raise "#{otp_version()} != #{@otp_version}"
    end

    bootstrap_otp()

    %{name: app_name, dir: app_dir} = release_app_config()
    log(:green, "creating", app_dir)
    File.mkdir_p!("#{app_dir}/Contents/Resources")

    rel_dir = Path.join([app_dir, "Contents", "Resources", "rel"])

    options = Keyword.fetch!(release.options, :app)

    info_plist =
      if info_plist_path = options[:info_plist_path] do
        EEx.eval_file(info_plist_path, assigns: [release: release])
      else
        default_info_plist(app_name, release.version)
      end

    File.write!("#{app_dir}/Contents/Info.plist", info_plist)
    File.cp_r!(release.path, rel_dir)

    launcher = options |> Keyword.fetch!(:launcher_path) |> File.read!()
    build_launcher(release, app_dir, app_name, launcher)

    for {destination, source} <- options[:resources] || [] do
      File.cp!(source, "#{app_dir}/Contents/Resources/#{destination}")
    end

    if options[:build_dmg] do
      build_dmg(release, app_name, options)
    end

    release
  end

  @doc false
  def bootstrap_otp do
    cache_dir = cache_dir()
    File.mkdir_p!(cache_dir)
    dir = "#{@elixirkit_dir}/otp-bootstrap"

    for target <- ~w(macos-aarch64 macos-x86_64) do
      cmd("sh", ~w(#{dir}/openssl/build.sh #{cache_dir} #{@openssl_version} #{target}))
    end

    cmd(
      "sh",
      ~w(#{dir}/otp/build.sh #{cache_dir} #{@otp_version} macos-universal #{@openssl_version})
    )
  end

  @doc false
  def otp_dir do
    "#{cache_dir()}/otp-#{@otp_version}-macos-universal"
  end

  defp build_launcher(release, app_dir, app_name, launcher) do
    app_tmp = "#{app_dir}/tmp"
    File.rm_rf!(app_tmp)
    File.mkdir_p!(app_tmp)
    File.write!("#{app_tmp}/Launcher.swift", launcher)

    launcher_dir = "#{app_dir}/Contents/MacOS"
    launcher_path = "#{launcher_dir}/#{app_name}"
    log(:green, "creating", launcher_path)
    File.mkdir_p!(launcher_dir)
    cache_dir = cache_dir()

    targets = [
      "macos-aarch64",
      "macos-x86_64"
    ]

    for target <- targets do
      otp_dir = "#{cache_dir}/otp-#{@otp_version}-#{target}"
      cc_target = cc_target(target)

      cmd("clang", [
        "-c",
        "-I",
        "#{otp_dir}/usr/include",
        "-target",
        cc_target,
        "-o",
        "#{app_tmp}/ElixirKit.o.#{target}",
        "#{@elixirkit_dir}/c_src/ElixirKit.m"
      ])

      cmd(
        "swiftc",
        [
          "-parse-as-library",
          "-import-objc-header",
          "#{@elixirkit_dir}/c_src/ElixirKit.h",
          "-lc++",
          "-ltermcap",
          "-target",
          cc_target,
          "-o",
          "#{app_tmp}/launcher.#{target}",
          "#{app_tmp}/Launcher.swift",
          "#{@elixirkit_dir}/c_src/ElixirKit.swift",
          "#{otp_dir}/usr/lib/liberl.a",
          "#{app_tmp}/ElixirKit.o.#{target}"
        ]
      )

      # this is a native runner, as opposed to simply a bash script, because the latter seems to
      # be always executed using Rosetta in an universal app.
      File.write!(
        "#{app_tmp}/runner.swift",
        EEx.eval_file("#{@elixirkit_dir}/c_src/runner.swift", release: release)
      )

      cmd(
        "swiftc",
        ~w(-o #{app_tmp}/runner.#{target} #{app_tmp}/runner.swift -target #{cc_target})
      )
    end

    cmd(
      "lipo",
      Enum.map(targets, &"#{app_tmp}/launcher.#{&1}") ++ ~w(-create -output #{launcher_path})
    )

    cmd(
      "lipo",
      Enum.map(targets, &"#{app_tmp}/runner.#{&1}") ++ ~w(-create -output #{launcher_path}Runner)
    )

    erts_dir = "#{app_dir}/Contents/Resources/rel/erts-#{release.erts_version}"

    if File.dir?(erts_dir) do
      erl_path = "#{erts_dir}/bin/erl"

      File.write!(erl_path, """
      #!/bin/sh
      SELF=$(readlink "$0" || true)
      if [ -z "$SELF" ]; then SELF="$0"; fi
      BINDIR="$(cd "$(dirname "$SELF")" && pwd -P)"
      ROOTDIR="${ERL_ROOTDIR:-"$(dirname "$(dirname "$BINDIR")")"}"
      EMU=beam
      PROGNAME=$(echo "$0" | sed 's/.*\\///')
      export EMU
      export ROOTDIR
      export BINDIR
      export PROGNAME
      RELEASE_ERLEXEC="${RELEASE_ERLEXEC:-"$BINDIR/erlexec"}"
      exec $RELEASE_ERLEXEC ${1+"$@"}
      """)
    end

    File.rm_rf!(app_tmp)

    cmd(
      "/System/Library/Frameworks/CoreServices.framework" <>
        "/Versions/A/Frameworks/LaunchServices.framework" <>
        "/Versions/A/Support/lsregister",
      ["-f", app_dir]
    )
  end

  defp cc_target("macos-aarch64"), do: "arm64-apple-macos11.0"
  defp cc_target("macos-x86_64"), do: "x86_64-apple-macos11.0"

  @doc false
  def open_release_app do
    %{name: name, dir: dir} = release_app_config()
    ensure_not_running(name)
    tty = tty()
    forward_logs("#{System.user_home!()}/Library/Logs/#{name}.log")
    cmd("open", ~w(-W --stdout #{tty} --stderr #{tty} #{dir}))
  end

  defp forward_logs(path) do
    File.write!(path, "")
    {:ok, log_pid} = File.open(path, [:read])

    Task.start_link(fn ->
      gets(log_pid)
    end)
  end

  defp gets(pid) do
    data = IO.gets(pid, "")

    if data != :eof do
      IO.write(data)
    end

    gets(pid)
  end

  defp ensure_not_running(name, kill? \\ true) do
    case System.cmd("pgrep", ["^#{name}$"], into: "") do
      {"", 1} ->
        :ok

      {out, 0} ->
        [pid] = String.split(out, "\n", trim: true)

        if kill? do
          log(:green, "killing", "#{name} (pid=#{pid})")
          cmd("kill", [pid])
        end

        Process.sleep(100)
        ensure_not_running(name, false)
    end
  end

  defp release_app_config do
    config = Mix.Project.config()
    name = Macro.camelize(Atom.to_string(config[:app]))
    dir = Path.join(Mix.Project.build_path(), "#{name}.app")
    %{name: name, dir: dir}
  end

  defp default_info_plist(app_name, version, extra \\ "") do
    """
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
      <key>CFBundleName</key>
      <string>#{app_name}</string>
      <key>CFBundleIdentifier</key>
      <string>#{app_name}</string>
      <key>CFBundleVersion</key>
      <string>#{version}</string>
      #{extra}
      <key>LSRequiresNativeExecution</key>
      <true/>
      <key>CFBundleExecutable</key>
      <string>#{app_name}Runner</string>
    </dict>
    </plist>
    """
  end

  defp build_dmg(release, app_name, options) do
    notarization = Keyword.fetch!(options, :notarization)

    dmg_dir = "#{Mix.Project.build_path()}/dmg"
    app_dir = "#{dmg_dir}/#{app_name}.app"
    File.rm_rf!(dmg_dir)
    File.mkdir_p!(dmg_dir)
    File.ln_s!("/Applications", "#{dmg_dir}/Applications")
    File.cp_r!("#{Mix.Project.build_path()}/#{app_name}.app", app_dir)

    log(:green, "signing", Path.relative_to_cwd(app_dir))
    to_sign = find_executable_files(app_dir) ++ [app_dir]
    codesign(to_sign, notarization)

    dmg_path = "#{Mix.Project.build_path()}/#{app_name}Install.dmg"
    log(:green, "creating", Path.relative_to_cwd(dmg_path))
    volname = "#{app_name}Install"
    cmd("hdiutil", ~w(create #{dmg_path} -ov -volname #{volname} -fs HFS+ -srcfolder #{dmg_dir}))

    log(:green, "notarizing", Path.relative_to_cwd(dmg_path))
    notarize(dmg_path, notarization)

    release
  end

  defp codesign(paths, options) do
    entitlements_plist_path = options |> Keyword.fetch!(:entitlements_plist_path) |> Path.expand()
    identity = Keyword.fetch!(options, :identity)

    flags = [
      "--force",
      "--timestamp",
      "--verbose",
      "4",
      "--options",
      "runtime",
      "--sign",
      identity,
      "--entitlements",
      entitlements_plist_path
    ]

    cmd("codesign", flags ++ paths)
  end

  defp notarize(path, options) do
    team_id = Keyword.fetch!(options, :team_id)
    apple_id = Keyword.fetch!(options, :apple_id)
    password = Keyword.fetch!(options, :password)

    cmd("xcrun", [
      "notarytool",
      "submit",
      "--team-id",
      team_id,
      "--apple-id",
      apple_id,
      "--password",
      password,
      "--progress",
      "--wait",
      path
    ])
  end

  defp log(color, command, message, options \\ []) do
    unless options[:quiet] do
      Mix.shell().info([color, "* #{command} ", :reset, message])
    end
  end

  defp find_executable_files(dir) do
    "find #{dir} -perm +111 -type f -exec sh -c \"file {} | grep --silent Mach-O\" \\; -print"
    |> sh()
    |> String.split("\n", trim: true)
  end

  defp tty do
    tty = sh("ps -p #{System.pid()} | tail -1 | awk '{ print $2 }'")
    "/dev/#{String.trim(tty)}"
  end

  defp cache_dir do
    if System.get_env("MIX_XDG") in ["1", "true"] do
      # XDG lookups are only done for linux OS
      :filename.basedir(:user_cache, "elixirkit", %{os: :linux})
    else
      :filename.basedir(:user_cache, "elixirkit")
    end
  end

  defp cmd(cmd, args, opts \\ []) do
    opts = Keyword.put_new(opts, :into, IO.stream())
    {out, result} = System.cmd(cmd, args, opts)

    if result != 0 do
      Mix.raise("""
      Command exited with #{result}

      cmd: #{cmd}
      args: #{inspect(args, pretty: true)}
      opts: #{inspect(opts, pretty: true)}
      """)
    end

    out
  end

  defp sh(cmd) do
    {out, result} = System.shell(cmd)

    if result != 0 do
      Mix.raise("""
      Command exited with #{result}

      cmd: #{cmd}
      """)
    end

    out
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
end

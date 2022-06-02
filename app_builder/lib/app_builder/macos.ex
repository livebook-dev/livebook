defmodule AppBuilder.MacOS do
  @moduledoc false

  import AppBuilder.Utils

  @templates_path "#{__ENV__.file}/../../templates"

  def bundle(release, options) do
    app_name = options[:name]

    app_path = "#{Mix.Project.build_path()}/#{app_name}.app"
    File.rm_rf!(app_path)
    tmp_dir = "#{Mix.Project.build_path()}/tmp"
    contents_path = "#{app_path}/Contents"
    resources_path = "#{contents_path}/Resources"

    copy_dir(release.path, "#{resources_path}/rel")

    launcher_eex_path = Path.expand("#{@templates_path}/macos/Launcher.swift.eex")
    launcher_src_path = "#{tmp_dir}/Launcher.swift"
    launcher_bin_path = "#{contents_path}/MacOS/#{app_name}Launcher"
    copy_template(launcher_eex_path, launcher_src_path, release: release, app_options: options)

    File.mkdir!("#{contents_path}/MacOS")
    log(:green, :creating, Path.relative_to_cwd(launcher_bin_path))

    cmd!("swiftc", [
      "-warnings-as-errors",
      "-target",
      swiftc_target(),
      "-o",
      launcher_bin_path,
      launcher_src_path
    ])

    icon_path =
      Keyword.get(options, :icon_path, Application.app_dir(:wx, "examples/demo/erlang.png"))

    dest_path = "#{resources_path}/AppIcon.icns"
    create_icon(icon_path, dest_path)

    for type <- Keyword.fetch!(options, :document_types) do
      if src_path = Keyword.get(type, :icon_path, icon_path) do
        dest_path = "#{resources_path}/#{type[:name]}Icon.icns"
        create_icon(src_path, dest_path)
      end
    end

    copy_template(
      Path.expand("#{@templates_path}/macos/Info.plist.eex"),
      "#{contents_path}/Info.plist",
      release: release,
      app_options: options
    )

    if options[:macos_build_dmg] do
      build_dmg(release, options)
    end

    release
  end

  defp build_dmg(release, options) do
    app_name = Keyword.fetch!(options, :name)
    notarization = Keyword.fetch!(options, :macos_notarization)

    dmg_dir = "#{Mix.Project.build_path()}/dmg"
    app_dir = "#{dmg_dir}/#{app_name}.app"
    tmp_dir = "#{Mix.Project.build_path()}/tmp"
    File.rm_rf!(dmg_dir)
    File.mkdir_p!(dmg_dir)

    File.ln_s!("/Applications", "#{dmg_dir}/Applications")

    File.cp_r!(
      "#{Mix.Project.build_path()}/#{app_name}.app",
      app_dir
    )

    to_sign =
      "#{app_dir}/**"
      |> Path.wildcard()
      |> Enum.filter(fn file ->
        stat = File.lstat!(file)
        Bitwise.band(0o100, stat.mode) != 0 and stat.type == :regular
      end)

    to_sign = to_sign ++ [app_dir]

    entitlements_eex_path = "#{Path.expand(@templates_path)}/macos/Entitlements.plist.eex"
    entitlements_plist_path = "#{tmp_dir}/Entitlements.plist"

    copy_template(entitlements_eex_path, entitlements_plist_path,
      release: release,
      app_options: options
    )

    log(:green, "signing", Path.relative_to_cwd(app_dir))
    codesign(to_sign, "--options=runtime --entitlements=#{entitlements_plist_path}", notarization)

    arch = :erlang.system_info(:system_architecture) |> to_string |> String.split("-") |> hd()
    vsn = release.version
    dmg_path = "#{Mix.Project.build_path()}/#{app_name}Install-#{vsn}-#{arch}.dmg"
    log(:green, "creating", Path.relative_to_cwd(dmg_path))

    cmd!(
      "hdiutil",
      ~w(create #{dmg_path} -ov -volname #{app_name}Install -fs HFS+ -srcfolder #{dmg_dir})
    )

    log(:green, "notarizing", Path.relative_to_cwd(dmg_path))
    notarize(dmg_path, notarization)

    release
  end

  defp codesign(paths, extra_flags, options) do
    identity = Keyword.fetch!(options, :identity)
    paths = Enum.join(paths, " ")
    flags = "--force --timestamp --verbose=4 --sign=\"#{identity}\" #{extra_flags}"
    shell!("codesign #{flags} #{paths}")
  end

  defp notarize(path, options) do
    team_id = Keyword.fetch!(options, :team_id)
    apple_id = Keyword.fetch!(options, :apple_id)
    password = Keyword.fetch!(options, :password)

    shell!("""
    xcrun notarytool submit \
      --team-id "#{team_id}" \
      --apple-id "#{apple_id}" \
      --password "#{password}" \
      --progress \
      --wait \
      #{path}
    """)
  end

  defp create_icon(src_path, dest_path) do
    log(:green, :creating, Path.relative_to_cwd(dest_path))
    src_path = normalize_icon_path(src_path)

    if Path.extname(src_path) == ".icns" do
      File.cp!(src_path, dest_path)
    else
      name = Path.basename(dest_path, ".icns")
      dest_tmp_path = "tmp/#{name}.iconset"
      File.rm_rf!(dest_tmp_path)
      File.mkdir_p!(dest_tmp_path)

      sizes = for(i <- [16, 32, 64, 128], j <- [1, 2], do: {i, j}) ++ [{512, 1}]

      for {size, scale} <- sizes do
        suffix =
          case scale do
            1 -> ""
            2 -> "@2x"
          end

        size = size * scale
        out = "#{dest_tmp_path}/icon_#{size}x#{size}#{suffix}.png"
        cmd!("sips", ~w(-z #{size} #{size} #{src_path} --out #{out}), into: "")
      end

      cmd!("iconutil", ~w(-c icns #{dest_tmp_path} -o #{dest_path}))
      File.rm_rf!(dest_tmp_path)
    end
  end

  defp swiftc_target do
    case :erlang.system_info(:system_architecture) do
      'x86_64' ++ _ ->
        "x86_64-apple-macosx10.15"

      'aarch64' ++ _ ->
        "arm64-apple-macosx12"
    end
  end
end

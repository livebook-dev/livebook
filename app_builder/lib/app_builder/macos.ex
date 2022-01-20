defmodule AppBuilder.MacOS do
  @moduledoc false

  import AppBuilder.Utils

  def build_mac_app_dmg(release, options) do
    {codesign, options} = Keyword.pop(options, :codesign)
    {notarize, options} = Keyword.pop(options, :notarize)

    release = build_mac_app(release, options)

    app_name = Keyword.fetch!(options, :name)
    File.rm_rf!("tmp/dmg")
    File.mkdir_p!("tmp/dmg")
    File.ln_s!("/Applications", "tmp/dmg/Applications")

    File.cp_r!(
      Path.join([Mix.Project.build_path(), "rel", "#{app_name}.app"]),
      "tmp/dmg/#{app_name}.app"
    )

    to_sign =
      "tmp/dmg/#{app_name}.app/**"
      |> Path.wildcard()
      |> Enum.filter(fn file ->
        stat = File.lstat!(file)
        Bitwise.band(0o100, stat.mode) != 0 and stat.type == :regular
      end)

    to_sign = to_sign ++ ["tmp/dmg/#{app_name}.app"]

    if codesign do
      entitlements_path = "tmp/entitlements.plist"

      File.write!(entitlements_path, """
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
      <plist version="1.0">
        <dict>
        <key>com.apple.security.cs.allow-jit</key>
        <true/>
        <key>com.apple.security.cs.allow-unsigned-executable-memory</key>
        <true/>
        <key>com.apple.security.cs.allow-dyld-environment-variables</key>
        <true/>
      </dict>
      </plist>
      """)

      codesign(to_sign, "--options=runtime --entitlements=#{entitlements_path}", codesign)
    end

    arch = :erlang.system_info(:system_architecture) |> to_string |> String.split("-") |> hd()
    vsn = release.version
    basename = "#{app_name}-#{vsn}-#{arch}.dmg"

    tmp_dmg_path = "tmp/#{app_name}.dmg"
    dmg_path = "#{Mix.Project.build_path()}/rel/#{basename}"

    File.rm_rf!(tmp_dmg_path)
    File.rm_rf!(dmg_path)

    cmd!(
      "hdiutil",
      ~w(create #{tmp_dmg_path} -ov -volname #{app_name}Install -fs HFS+ -srcfolder tmp/dmg)
    )

    cmd!(
      "hdiutil",
      ~w(convert #{tmp_dmg_path} -format UDZO -o #{dmg_path})
    )

    if codesign do
      codesign([dmg_path], "", codesign)
    end

    if notarize do
      notarize(dmg_path, notarize)
    end

    File.rm!(tmp_dmg_path)
    release
  end

  defp codesign(paths, args, options) do
    identity = Keyword.fetch!(options, :identity)
    paths = Enum.join(paths, " ")
    shell!("codesign --force --timestamp --verbose=4 --sign=\"#{identity}\" #{args} #{paths}")
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

  def build_mac_app(release, options) do
    options =
      Keyword.validate!(options, [
        :name,
        :version,
        :launcher_script,
        :logo_path,
        :info_plist,
        :url_schemes,
        :document_types
      ])

    app_name = Keyword.fetch!(options, :name)

    app_bundle_path = Path.join([Mix.Project.build_path(), "rel", "#{app_name}.app"])
    File.rm_rf!(app_bundle_path)
    File.mkdir_p!(Path.join([app_bundle_path, "Contents", "Resources"]))
    File.rename!(release.path, Path.join([app_bundle_path, "Contents", "Resources", "rel"]))

    launcher_script = options[:launcher_script] || launcher_script(release.name, app_name)
    launcher_script_path = Path.join([app_bundle_path, "Contents", "MacOS", app_name])
    File.mkdir_p!(Path.dirname(launcher_script_path))
    File.write!(launcher_script_path, launcher_script)
    File.chmod!(launcher_script_path, 0o700)

    logo_path = options[:logo_path] || Application.app_dir(:wx, "examples/demo/erlang.png")
    create_logo(app_bundle_path, logo_path)

    info_plist = options[:info_plist] || build_info_plist(options)
    File.write!(Path.join([app_bundle_path, "Contents", "Info.plist"]), info_plist)

    release
  end

  defp launcher_script(release_name, app_name) do
    """
    #!/bin/sh
    set -e
    root=$(dirname $(dirname "$0"))
    $root/Resources/rel/bin/#{release_name} start \\
      1>> ~/Library/Logs/#{app_name}.stdout.log \\
      2>> ~/Library/Logs/#{app_name}.stderr.log
    """
  end

  defp build_info_plist(options) do
    app_name = Keyword.fetch!(options, :name)
    app_version = Keyword.fetch!(options, :version)

    url_schemes =
      """
      \n<key>CFBundleURLTypes</key>
      <array>
      """ <>
        for scheme <- options[:url_schemes] || [], into: "" do
          """
          <dict>
            <key>CFBundleURLName</key>
            <string>#{app_name}</string>
            <key>CFBundleURLSchemes</key>
            <array>
              <string>#{scheme}</string>
            </array>
          </dict>
          """
        end <>
        "</array>"

    document_types =
      """
      \n<key>CFBundleDocumentTypes</key>
      <array>
      """ <>
        for type <- options[:document_types] || [], into: "" do
          """
          <dict>
            <key>CFBundleTypeName</key>
            <string>#{type.name}</string>
            <key>CFBundleTypeRole</key>
            <string>#{type.role}</string>
            <key>CFBundleTypeExtensions</key>
            <array>
              #{for ext <- type.extensions, do: "<string>#{ext}</string>"}
            </array>
          </dict>
          """
        end <>
        "</array>"

    """
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleName</key>
    <string>#{app_name}</string>
    <key>CFBundleDisplayName</key>
    <string>#{app_name}</string>
    <key>CFBundleShortVersionString</key>
    <string>#{app_version}</string>
    <key>CFBundleVersion</key>
    <string>#{app_version}</string>
    <key>CFBundleIconFile</key>
    <string>AppIcon</string>
    <key>CFBundleIconName</key>
    <string>AppIcon</string>#{url_schemes}#{document_types}
    </dict>
    </plist>
    """
  end

  defp create_logo(app_bundle_path, logo_source_path) do
    logo_dest_path = Path.join([app_bundle_path, "Contents", "Resources", "AppIcon.icns"])

    if Path.extname(logo_source_path) == ".icns" do
      File.cp!(logo_source_path, logo_dest_path)
    else
      logo_dest_tmp_path = "tmp/AppIcon.iconset"
      File.rm_rf!(logo_dest_tmp_path)
      File.mkdir_p!(logo_dest_tmp_path)

      sizes = for(i <- [16, 32, 64, 128], j <- [1, 2], do: {i, j}) ++ [{512, 1}]

      for {size, scale} <- sizes do
        suffix =
          case scale do
            1 -> ""
            2 -> "@2x"
          end

        size = size * scale
        out = "#{logo_dest_tmp_path}/icon_#{size}x#{size}#{suffix}.png"
        cmd!("sips", ~w(-z #{size} #{size} #{logo_source_path} --out #{out}))
      end

      cmd!("iconutil", ~w(-c icns #{logo_dest_tmp_path} -o #{logo_dest_path}))
      File.rm_rf!(logo_dest_tmp_path)
    end
  end
end

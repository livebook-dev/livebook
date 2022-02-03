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
      File.write!(entitlements_path, entitlements())
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
        :logo_path,
        :info_plist,
        :url_schemes,
        :document_types,
        :additional_paths
      ])

    app_name = Keyword.fetch!(options, :name)
    additional_paths = Keyword.get(options, :additional_paths, [])

    app_bundle_path = Path.join([Mix.Project.build_path(), "rel", "#{app_name}.app"])
    File.rm_rf!(app_bundle_path)
    File.mkdir_p!(Path.join([app_bundle_path, "Contents", "Resources"]))
    File.rename!(release.path, Path.join([app_bundle_path, "Contents", "Resources", "rel"]))

    File.mkdir_p!("tmp")
    launcher_src_path = "tmp/Launcher.swift"
    File.write!(launcher_src_path, launcher(additional_paths))
    launcher_path = Path.join([app_bundle_path, "Contents", "MacOS", app_name <> "Launcher"])
    File.mkdir_p!(Path.dirname(launcher_path))

    cmd!("swiftc", [
      "-warnings-as-errors",
      "-target",
      swiftc_target(),
      "-o",
      launcher_path,
      launcher_src_path
    ])

    logo_path = options[:logo_path] || Application.app_dir(:wx, "examples/demo/erlang.png")
    create_logo(app_bundle_path, logo_path)

    info_plist = options[:info_plist] || info_plist(options)
    File.write!(Path.join([app_bundle_path, "Contents", "Info.plist"]), info_plist)

    release
  end

  defp launcher(additional_paths) do
    additional_paths = additional_paths
      |> Enum.map(&("\\(resourcePath)#{&1}"))
      |> Enum.join(":")

    """
    import Foundation
    import Cocoa

    let fm = FileManager.default
    let appName = Bundle.main.object(forInfoDictionaryKey: "CFBundleDisplayName") as! String
    let home = NSHomeDirectory()

    let logPath = "\\(home)/Library/Logs/\\(appName).log"
    if !fm.fileExists(atPath: logPath) { fm.createFile(atPath: logPath, contents: Data()) }
    let logFile = FileHandle(forUpdatingAtPath: logPath)
    logFile?.seekToEndOfFile()

    let releaseScriptPath = Bundle.main.path(forResource: "rel/bin/mac_app", ofType: "")!

    let resourcePath = Bundle.main.resourcePath ?? ""
    let additionalPaths = "#{additional_paths}"

    var environment = ProcessInfo.processInfo.environment
    let path = environment["PATH"] ?? ""

    environment["PATH"] = "\\(additionalPaths):\\(path)"

    let task = Process()
    task.environment = environment
    task.launchPath = releaseScriptPath
    task.arguments = ["start"]
    task.standardOutput = logFile
    task.standardError = logFile
    try task.run()

    task.waitUntilExit()

    if task.terminationStatus != 0 {
      let alert = NSAlert()
      alert.alertStyle = .critical
      alert.messageText = "\\(appName) exited with error status \\(task.terminationStatus)."
      alert.informativeText = "Logs available at \\(logPath)."
      alert.runModal()
    }
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

  defp swiftc_target do
    case :erlang.system_info(:system_architecture) do
      'x86_64' ++ _ ->
        "x86_64-apple-macosx10.15"

      'aarch64' ++ _ ->
        "arm64-apple-macosx12"
    end
  end

  ## Templates

  require EEx

  defp entitlements do
    """
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
    """
  end

  code = """
  <%
    app_name = Keyword.fetch!(options, :name)
    app_version = Keyword.fetch!(options, :version)
  %>
  <?xml version="1.0" encoding="UTF-8"?>
  <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
  <plist version="1.0">
  <dict>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleExecutable</key>
    <string><%= app_name %>Launcher</string>
    <key>CFBundleName</key>
    <string><%= app_name %></string>
    <key>CFBundleDisplayName</key>
    <string><%= app_name %></string>
    <key>CFBundleShortVersionString</key>
    <string><%= app_version %></string>
    <key>CFBundleVersion</key>
    <string><%= app_version %></string>
    <key>CFBundleIconFile</key>
    <string>AppIcon</string>
    <key>CFBundleIconName</key>
    <string>AppIcon</string>

  <%= if schemes = options[:url_schemes] do %>
    <key>CFBundleURLTypes</key>
    <array>
    <%= for scheme <- schemes do %>
      <dict>
        <key>CFBundleURLName</key>
        <string><%= app_name %></string>
        <key>CFBundleURLSchemes</key>
        <array>
          <string><%= scheme %></string>
        </array>
      </dict>
    <% end %>
    </array>
  <% end %>

  <%= if types = options[:document_types] do %>
    <key>CFBundleDocumentTypes</key>
    <array>
    <%= for type <- types do %>
      <dict>
        <key>CFBundleTypeName</key>
        <string><%= type.name %></string>
        <key>CFBundleTypeRole</key>
        <string><%= type.role %></string>
        <key>CFBundleTypeExtensions</key>
        <array>
        <%= for ext <- type.extensions do %>
          <string><%= ext %></string>
        <% end %>
        </array>
      </dict>
    <% end %>
    </array>
  <% end %>

    </dict>
  </plist>
  """

  EEx.function_from_string(:defp, :info_plist, code, [:options], trim: true)
end

defmodule AppBundler.Windows do
  @moduledoc false

  import AppBundler.Utils

  @templates_path "#{__ENV__.file}/../../templates"

  def bundle(release, options) do
    {:ok, _} = Application.ensure_all_started(:ssl)
    {:ok, _} = Application.ensure_all_started(:inets)

    app_name = options[:name]

    app_path = "#{Mix.Project.build_path()}/#{app_name}-win"
    File.rm_rf!(app_path)

    copy_dir(release.path, "#{app_path}/rel")

    manifest_eex_path = Path.expand("#{@templates_path}/windows/Manifest.xml.eex")
    manifest_xml_path = "#{app_path}/Manifest.xml"
    copy_template(manifest_eex_path, manifest_xml_path, release: release)

    rcedit_path = ensure_rcedit()
    erl_exe = "#{app_path}/rel/erts-#{release.erts_version}/bin/erl.exe"
    log(:green, :updating, Path.relative_to_cwd(erl_exe))
    cmd!(rcedit_path, ["--application-manifest", manifest_xml_path, erl_exe])

    vcredist_path = ensure_vcredistx64()
    copy_file(vcredist_path, "#{app_path}/vcredist_x64.exe")

    icon_path = options[:icon_path]

    if icon_path do
      create_icon(icon_path, "#{app_path}/AppIcon.ico")
    end

    tmp_dir = "#{Mix.Project.build_path()}/tmp"
    File.mkdir_p!(tmp_dir)
    launcher_eex_path = Path.expand("#{@templates_path}/windows/Launcher.vb.eex")
    launcher_src_path = "#{tmp_dir}/Launcher.vb"
    launcher_bin_path = "#{app_path}/#{app_name}Launcher.exe"
    copy_template(launcher_eex_path, launcher_src_path, release: release, app_options: options)
    File.mkdir!("#{app_path}/Logs")

    args = [
      path(launcher_src_path),
      "/out:" <> path(launcher_bin_path),
      "/nologo",
      "/target:winexe",
      "/win32manifest:" <> path(manifest_xml_path)
    ]

    extra_args =
      if icon_path do
        ["/win32icon:" <> path("#{app_path}/AppIcon.ico")]
      else
        []
      end

    vbc_path = ensure_vbc()

    cmd!(vbc_path, args ++ extra_args)

    for type <- Keyword.fetch!(options, :document_types) do
      if src_path = Keyword.get(type, :icon_path, icon_path) do
        dest_path = "#{app_path}/#{type[:name]}Icon.ico"
        create_icon(src_path, dest_path)
      end
    end

    if Keyword.fetch!(options, :build_installer) do
      installer_eex_path = Path.expand("#{@templates_path}/windows/Installer.nsi.eex")
      installer_nsi_path = "#{app_path}/Installer.nsi"
      copy_template(installer_eex_path, installer_nsi_path, release: release, app_options: options)
      makensis_path = ensure_makensis()
      log(:green, "creating", Path.relative_to_cwd("#{app_path}/#{app_name}Install.exe"))
      cmd!(makensis_path, [installer_nsi_path])
    end

    release
  end

  defp path(path), do: String.replace(path, "/", "\\")

  def handle_event(module, input)

  def handle_event(module, "open_url:" <> url) do
    module.open_url(url)
  end

  def handle_event(module, "open_file:" <> path) do
    module.open_file(String.replace(path, "\\", "/"))
  end

  defp ensure_vcredistx64 do
    url = "https://aka.ms/vs/17/release/vc_redist.x64.exe"
    AppBundler.Utils.ensure_executable(url)
  end

  defp ensure_makensis do
    url = "https://downloads.sourceforge.net/project/nsis/NSIS%203/3.08/nsis-3.08.zip"
    sha256 = "1bb9fc85ee5b220d3869325dbb9d191dfe6537070f641c30fbb275c97051fd0c"
    AppBundler.Utils.ensure_executable(url, sha256, "nsis-3.08/makensis.exe")
  end

  defp ensure_rcedit do
    url = "https://github.com/electron/rcedit/releases/download/v1.1.1/rcedit-x64.exe"
    sha256 = "02e8e8c5d430d8b768980f517b62d7792d690982b9ba0f7e04163cbc1a6e7915"
    AppBundler.Utils.ensure_executable(url, sha256)
  end

  defp ensure_magick do
    System.find_executable("magick.exe") ||
      raise "couldn't find magick.exe in PATH to automatically convert images to .ico"
  end

  def ensure_vbc do
    case System.shell("dir %WINDIR%\\Microsoft.NET\\Framework64\\vbc.exe /s/b") do
      {paths, 0} ->
        paths |> String.split("\r\n", trim: true) |> List.last()

      {_, 1} ->
        raise "cannot find vbc.exe. You need to install Visual Studio."
    end
  end

  defp create_icon(src_path, dest_path) do
    log(:green, "creating", Path.relative_to_cwd(dest_path))
    src_path = normalize_icon_path(src_path)

    if Path.extname(src_path) == ".ico" do
      File.cp!(src_path, dest_path)
    else
      magick_path = ensure_magick()
      sizes = [16, 32, 48, 64, 128]

      for i <- sizes do
        cmd!(magick_path, [src_path, "-resize", "#{i}x#{i}", sized_path(dest_path, i)])
      end

      sized_paths = Enum.map(sizes, &sized_path(dest_path, &1))
      cmd!(magick_path, sized_paths ++ [dest_path])
    end
  end

  defp sized_path(path, size) do
    String.replace_trailing(path, ".ico", ".#{size}.ico")
  end
end
